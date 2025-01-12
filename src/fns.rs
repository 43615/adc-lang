//! Pure functions and polymorphic execution engine
//! 
//! Functions have 1, 2, or 3 `&Value` parameters (monadic, dyadic, triadic) and a boolean mode switch (false by default, enabled by \` command)

#![allow(dead_code)]	//TODO: make not dead

use std::collections::{VecDeque};
use std::ptr::NonNull;
use malachite::{Integer, Rational};
use malachite::num::arithmetic::traits::{Pow, Reciprocal};
use malachite::num::basic::traits::{NegativeOne, Zero};
use malachite::num::conversion::traits::{RoundingFrom, RoundingInto};
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
		#[inline(always)] pub fn $name(a: &Value, m: bool) -> Result<Value, FnErr> {
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

//TODO: remove
mon!(ph1);

//TODO: remove
dya!(ph2);

//TODO: remove
tri!(ph3);


/// execute monadic fn, automatic iteration
pub fn exec1(f: Mon, a: &Value, m: bool) -> Result<Value, FnErr> {
	if let A(aa) = a { unsafe {	//iterate through array, bfs without recursion
		//NonNull pointers are required because of aliasing rules, soundness is ensured manually
		let mut az: Vec<Value> = Vec::new();	//resulting array
		let mut q: VecDeque<(&Vec<Value>, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays
		q.push_back((aa, (&mut az).into()));
		while let Some((src, mut dst)) = q.pop_front() {	//keep reading from front of queue
			for val in src {	//iter current layer
				if let A(nsrc) = val {	//array encountered, pseudorecursion needed
					dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
					let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
					q.push_back((nsrc, ndst.into()));	//add next layer vecs to queue
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

/// execute dyadic fn, automatic iteration
pub(crate) fn exec2(f: Dya, a: &Value, b: &Value, m: bool) -> Result<Value, FnErr> {
	
	todo!()
}

/// execute triadic fn, automatic iteration
pub(crate) fn exec3(f: Tri, a: &Value, b: &Value, c: &Value, m: bool) -> Result<Value, FnErr> {

	todo!()
}

dya!(add
	B(ba), B(bb), _ => {	//boolean or (packed)
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
	
	N(na), N(nb), _ => Ok(N(na + nb))	//add numbers
	
	S(sa), S(sb), _ => Ok(S(sa.to_owned() + sb))	//concat strings
);

dya!(sub
	N(na), N(nb), _ => Ok(N(na - nb))	//subtract numbers
);

dya!(mul
	B(ba), B(bb), _ => {	//boolean and (packed)
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

	N(na), N(nb), _ => Ok(N(na * nb))	//multiply numbers
	
	S(sa), N(nb), _ => {	//repeat string
		let ib = rti1(nb);
		match usize::try_from(&ib) {
			Ok(ub) if sa.len().checked_mul(ub).is_some() => Ok(S(sa.repeat(ub))),
			_ => Err(Index(ib.to_owned()))
		}
	}
);

dya!(div
	N(na), N(nb), _ => {	//divide numbers
		if *nb != Rational::ZERO {
			Ok(N(na / nb))
		}
		else {
			Err(Arith("division by 0".into()))
		}		
	}
);

mon!(inv	
	B(ba), _ => Ok(B({let mut res = ba.to_owned(); res.negate(); res}))	//negate boolean
	
	N(na), _ => Ok(N(na.reciprocal()))	//reciprocate number
	
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
	
	N(na), N(nb), _ => {	//raise number to power
		let (fa, fb) = rtf2(na, nb);
		ftr1(fa.powf(fb)).map(N)
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
		if let Some((bidx, len)) = re.find(sa).map(|m| (m.start(), m.len())) {	//byte position
			Ok(A(vec![
				N(sa.char_indices().position(|(cidx, _)| cidx==bidx).unwrap().into()),	//char position
				N(len.into())	//length of match
			]))
		}
		else {
			Ok(N(Rational::NEGATIVE_ONE))
		}
	}
);
