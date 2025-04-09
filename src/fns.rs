//! Pure functions and polymorphic execution engine
//! 
//! Functions have 1, 2, or 3 `&Value` parameters (monadic, dyadic, triadic) and a boolean mode switch (false by default, enabled by \` command)

#![allow(dead_code)]	//TODO: make not dead

use std::collections::{VecDeque};
use std::ptr::NonNull;
use malachite::{Integer, rational::Rational};
use malachite::base::num::arithmetic::traits::{DivRem, Mod, ModInverse, ModPow, Pow, Reciprocal};
use malachite::base::num::basic::traits::{NegativeOne, Zero};
use malachite::base::num::conversion::traits::{RoundingFrom, RoundingInto};
use regex::Regex;
use crate::structs::Value::{self, *};
use crate::errors::FnErr::{self, *};
use crate::conv::*;
use crate::RE_CACHE;

/// Monadic function definition
pub(crate) type Mon = fn(&Value, bool) -> Result<Value, FnErr>;
/// Monadic template with standard type matching
macro_rules! mon {
    ($name:ident $($pa:pat, $m:pat => $op:expr)*) => {
		#[inline(always)] pub(crate) fn $name(a: &Value, m: bool) -> Result<Value, FnErr> {
			match (a,m) {
				$(($pa, $m) => $op,)*
				_ => Err(Type1(a.into()))
			}
		}
	}
}

/// Dyadic function definition
pub(crate) type Dya = fn(&Value, &Value, bool) -> Result<Value, FnErr>;
/// Dyadic template with standard type matching
macro_rules! dya {
    ($name:ident $($pa:pat, $pb:pat, $m:pat => $op:expr)*) => {
		#[inline(always)] pub(crate) fn $name(a: &Value, b: &Value, m: bool) -> Result<Value, FnErr> {
			match (a,b,m) {
				$(($pa, $pb, $m) => $op,)*
				_ => Err(Type2(a.into(), b.into()))
			}
		}
	}
}

/// Triadic function definition
pub(crate) type Tri = fn(&Value, &Value, &Value, bool) -> Result<Value, FnErr>;
/// Triadic template with standard type matching
macro_rules! tri {
    ($name:ident $($pa:pat, $pb:pat, $pc:pat, $m:pat => $op:expr)*) => {
		#[inline(always)] pub(crate) fn $name(a: &Value, b: &Value, c: &Value, m: bool) -> Result<Value, FnErr> {
			match (a,b,c,m) {
				$(($pa, $pb, $pc, $m) => $op,)*
				_ => Err(Type3(a.into(), b.into(), c.into()))
			}
		}
	}
}

/// execute monadic fn, pseudorecursion through nested arrays
pub(crate) fn exec1(f: Mon, a: &Value, m: bool) -> Result<Value, FnErr> {
	if let A(aa) = a { unsafe {	//iterate through array, bfs without recursion
		//NonNull pointers are required because of aliasing rules, soundness is ensured manually
		let mut az: Vec<Value> = Vec::new();	//resulting array
		let mut q: VecDeque<(&Vec<Value>, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays
		q.push_back((aa, (&mut az).into()));
		while let Some((src, mut dst)) = q.pop_front() {	//keep reading from front of queue
			for val in src {	//iterate through current layer
				if let A(nsrc) = val {	//array encountered, pseudorecursion needed
					dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
					let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
					q.push_back((nsrc, ndst.into()));	//add nested source and destination arrays to queue
				}
				else {	//plain value, compute and store result
					dst.as_mut().push(f(val, m)?);
				}
			}
		}
		Ok(A(az))
	}}
	else {	//just call function
		f(a, m)
	}
}

/// execute dyadic fn, pseudorecursion through nested arrays
pub(crate) fn exec2(f: Dya, a: &Value, b: &Value, m: bool) -> Result<Value, FnErr> {
	match (a, b) {
		(A(_), A(_)) | (A(_), _) | (_, A(_)) => { unsafe {	//iterate through array(s), bfs without recursion
			//NonNull pointers are required because of aliasing rules, soundness is ensured manually
			let mut az: Vec<Value> = Vec::new();	//resulting array
			let mut q: VecDeque<(&Value, &Value, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays, plain values in 0,1 get promoted later
			q.push_back((a, b, (&mut az).into()));
			while let Some((a, b, mut dst)) = q.pop_front() {	//keep reading from front of queue
				for (va, vb) in {lenck2(a, b)?; PromotingIter::from(a).zip(PromotingIter::from(b))} {
					match (va, vb) {
						(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//one or both elements are arrays
							dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
							let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
							q.push_back((va, vb, ndst.into()));	//add nested source and destination arrays to queue
						}
						(_, _) => {	//plain value, compute and store result
							dst.as_mut().push(f(va, vb, m)?);
						}
					}
				}
			}
			Ok(A(az))
		}}
		(_, _) => {	//just call function
			f(a, b, m)
		}
	}
}

/// execute triadic fn, pseudorecursion through nested arrays
pub(crate) fn exec3(f: Tri, a: &Value, b: &Value, c: &Value, m: bool) -> Result<Value, FnErr> {
	match (a, b, c) {
		(A(_), A(_), A(_)) |
		(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
		(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => { unsafe {	//iterate through array(s), bfs without recursion
			//NonNull pointers are required because of aliasing rules, soundness is ensured manually
			let mut az: Vec<Value> = Vec::new();	//resulting array
			let mut q: VecDeque<(&Value, &Value, &Value, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays, plain values in 0,1,2 get promoted later
			q.push_back((a, b, c, (&mut az).into()));
			while let Some((a, b, c, mut dst)) = q.pop_front() {    //keep reading from front of queue
				for ((va, vb), vc) in {lenck3(a, b, c)?; PromotingIter::from(a).zip(PromotingIter::from(b)).zip(PromotingIter::from(c))} {
					match (va, vb, vc) {
						(A(_), A(_), A(_)) |
						(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
						(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
							dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
							let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
							q.push_back((va, vb, vc, ndst.into()));	//add nested source and destination arrays to queue
						}
						(_, _, _) => {	//plain value, compute and store result
							dst.as_mut().push(f(va, vb, vc, m)?);
						}
					}
				}
			}
			Ok(A(az))
		}}
		(_, _, _) => {	//just call function
			f(a, b, c, m)
		}
	}
}

dya!(add
	B(ba), B(bb), _ => {	//concat booleans
		let mut res = ba.to_owned();
		res.append(&mut bb.to_owned());
		Ok(B(res))
	}

	N(ra), N(rb), _ => Ok(N(ra + rb))	//add numbers
	
	S(sa), S(sb), _ => Ok(S(sa.to_owned() + sb))	//concat strings
);

dya!(sub
	B(ba), B(bb), _ => {	//subtract booleans (a & !b)
		if ba.len() == bb.len() {
			let mut res = ba.to_owned();
			res.difference(bb);
			Ok(B(res))
		}
		else {
			Err(Arith(
				format!("booleans of different lengths: {}, {}", ba.len(), bb.len())
			))
		}
	}
	
	N(ra), N(rb), _ => Ok(N(ra - rb))	//subtract numbers
);

dya!(mul
	B(ba), B(bb), _ => {	//boolean and
		if ba.len() == bb.len() {
			let mut res = ba.to_owned();
			res.and(bb);
			Ok(B(res))
		}
		else {
			Err(Arith(
				format!("booleans of different lengths: {}, {}", ba.len(), bb.len())
			))
		}
	}

	N(ra), N(rb), _ => Ok(N(ra * rb))	//multiply numbers
	
	S(sa), N(rb), _ => {	//repeat string
		let ib = r_i1(rb);
		match usize::try_from(&ib) {
			Ok(ub) if sa.len().checked_mul(ub).is_some() => Ok(S(sa.repeat(ub))),
			_ => Err(Index(ib.to_owned()))
		}
	}
);

dya!(div
	B(ba), B(bb), _ => {	//boolean or
		if ba.len() == bb.len() {
			let mut res = ba.to_owned();
			res.or(bb);
			Ok(B(res))
		}
		else {
			Err(Arith(
				format!("booleans of different lengths: {}, {}", ba.len(), bb.len())
			))
		}
	}
	
	N(ra), N(rb), _ => {	//divide numbers
		if *rb != Rational::ZERO {
			Ok(N(ra / rb))
		}
		else {
			Err(Arith("division by 0".into()))
		}
	}
);

mon!(inv	
	B(ba), _ => Ok(B({let mut res = ba.to_owned(); res.negate(); res}))	//negate boolean
	
	N(ra), _ => Ok(N(ra.reciprocal()))	//reciprocate number
	
	S(sa), _ => Ok(S(sa.chars().rev().collect()))	//reverse string
);

dya!(pow
	B(ba), B(bb), _ => {	//boolean xor
		if ba.len() == bb.len() {
			let mut res = ba.to_owned();
			res.xor(bb);
			Ok(B(res))
		}
		else {
			Err(Arith(
				format!("booleans of different lengths: {}, {}", ba.len(), bb.len())
			))
		}
	}
	
	N(ra), N(rb), _ => {	//raise number to power
		let (fa, fb) = r_f2(ra, rb);
		f_r1(fa.powf(fb)).map(N)
	}
	
	S(sa), S(sb), false => {	//find substring by literal
		if let Some(bidx) = sa.find(sb) {	//byte position
			Ok(N(sa.char_indices().position(|(cidx, _)| cidx==bidx).unwrap().into()))	//char position
		}
		else {
			Ok(N(Rational::NEGATIVE_ONE))
		}
	}
	
	S(sa), S(sb), true => {	//find substring by regex
		let re = RE_CACHE.get(sb).map_err(Custom)?;
		if let Some(m) = re.find(sa) {	//find match
			Ok(A(vec![
				N(sa.char_indices().position(|(cidx, _)| cidx==m.start()).unwrap().into()),	//char position of start
				N(m.as_str().chars().count().into())	//char length of match
			]))
		}
		else {
			Ok(N(Rational::NEGATIVE_ONE))
		}
	}
);

mon!(log
	B(ba), _ => Ok(N(ba.len().into()))	//length of boolean
	
	N(ra), _ => f_r1(r_f1(ra).ln()).map(N)	//natural log of number
	
	S(sa), false => Ok(N(sa.chars().count().into()))	//char length of string
	
	S(sa), true => Ok(N(sa.len().into()))	//byte length of string
);

dya!(logb
	N(ra), N(rb), _ => {	//base b log of a
		let (fa, fb) = r_f2(ra, rb);
		f_r1(fa.log(fb)).map(N)
	}
);

dya!(r#mod
	N(ra), N(rb), _ => {	//modulo
		let (ia, ib) = r_i2(ra, rb);
		if ib != Rational::ZERO {
			Ok(N(ia.mod_op(ib).into()))
		}
		else {
			Err(Arith("reduction mod 0".into()))
		}
	}

	S(sa), N(rb), _ => {	//extract char
		let ib = r_i1(rb);
		if let Ok(ub) = usize::try_from(&ib) {
			Ok(S(sa.chars().nth(ub).map(|c| c.into()).unwrap_or_default()))
		}
		else {
			Err(Index(ib))
		}
	}
);

dya!(euc
	B(ba), N(rb), _ => {	//split boolean
		let ib = r_i1(rb);
		if let Ok(ub) = usize::try_from(&ib) {
			if ub < ba.len() {
				let mut by = ba.to_owned();
				let bz = by.split_off(ub);
				Ok(A(vec![B(by), B(bz)]))
			}
			else {
				Err(Index(ib))
			}
		}
		else {
			Err(Index(ib))
		}
	}

	N(ra), N(rb), _ => {	//euclidean division
		let (ia, ib) = r_i2(ra, rb);
		if ib != Rational::ZERO {
			let (quot, rem) = ia.div_rem(ib);
			Ok(A(vec![
				N(quot.into()),
				N(rem.into())
			]))
		}
		else {
			Err(Arith("reduction mod 0".into()))
		}
	}

	S(sa), N(rb), _ => {	//split string at char
		let ib = r_i1(rb);
		if let Ok(ub) = usize::try_from(&ib) {
			let bidx = sa.char_indices().position(|(cidx, _)| cidx==ub).unwrap();
			if bidx < sa.len() {
				let mut sy = sa.to_owned();
				let sz = sy.split_off(bidx);
				Ok(A(vec![S(sy), S(sz)]))
			}
			else {
				Err(Index(ib))
			}
		}
		else {
			Err(Index(ib))
		}
	}
);

tri!(bar
	B(ba), b, c, _ => {	//selection (a ? b : c)
		if ba.len() == 1 {	//plain value
			if ba[0] {Ok(b.to_owned())} else {Ok(c.to_owned())}
		}
		else {	//array of values for each bit
			Ok(A(
				ba.iter().map(|bit| if bit {b.to_owned()} else {c.to_owned()}).collect()
			))
		}
	}

	N(ra), N(rb), N(rc), _ => {	//modular exponentiation (a ^ b mod c)
		let (mut na, nb, nc) = r_n3(ra, rb, rc);
		if rb < &Rational::ZERO {	//find inverse if exponent is negative
			na = (&na % &nc).mod_inverse(&nc).ok_or_else(|| Arith(format!("{na} doesn't have an inverse mod {nc}")))?;
		}
		Ok(N((na % &nc).mod_pow(nb, nc).into()))
	}
	
	S(sa), S(sb), S(sc), false => Ok(S(sa.replace(sb, sc)))	//replace substrings by literal
	
	S(sa), S(sb), S(sc), true => {	//replace substrings by regex
		let re = RE_CACHE.get(sb).map_err(Custom)?;
		Ok(S(re.replace_all(sa, sc).into()))
	}
);