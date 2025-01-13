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

//TODO: remove
mon!(ph1);

//TODO: remove
dya!(ph2);

//TODO: remove
tri!(ph3);


/// execute monadic fn, recurse through arrays
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

/// execute dyadic fn, recurse through arrays
pub(crate) fn exec2(f: Dya, a: &Value, b: &Value, m: bool) -> Result<Value, FnErr> {
	match (a, b) {
		(A(_), A(_)) | (A(_), _) | (_, A(_)) => { unsafe {	//iterate through array(s), bfs without recursion
			//NonNull pointers are required because of aliasing rules, soundness is ensured manually
			let mut az: Vec<Value> = Vec::new();	//resulting array
			let mut q: VecDeque<(&Value, &Value, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays
			q.push_back((a, b, (&mut az).into()));
			while let Some((a, b, mut dst)) = q.pop_front() {	//keep reading from front of queue
				match (a, b) {
					(A(aa), A(ab)) => {	//current entry is two arrays
						if aa.len() == ab.len() {	//check lengths
							for (va, vb) in aa.iter().zip(ab) {	//iterate through both at once
								match (va, vb) {
									(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//one or both elements are arrays
										dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
										let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
										q.push_back((va, vb, ndst.into()));	//add next layer vecs to queue
									}
									(_, _) => {	//plain value, compute and store result
										dst.as_mut().push(f(va, vb, m)?);
									}
								}
							}
						}
						else {
							return Err(Len2(aa.len(), ab.len()));
						}
					}
					(A(aa), vb) => {	//current entry is array + value
						for va in aa {
							match (va, vb) {
								(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//one or both elements are arrays
									dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
									let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
									q.push_back((va, vb, ndst.into()));	//add next layer vecs to queue
								}
								(_, _) => {	//plain value, compute and store result
									dst.as_mut().push(f(va, vb, m)?);
								}
							}
						}
					}
					(va, A(ab)) => {	//current entry is value + array
						for vb in ab {
							match (va, vb) {
								(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//one or both elements are arrays
									dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
									let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
									q.push_back((va, vb, ndst.into()));	//add next layer vecs to queue
								}
								(_, _) => {	//plain value, compute and store result
									dst.as_mut().push(f(va, vb, m)?);
								}
							}
						}
					}
					(_, _) => {	//current entry is two values
						unreachable!();
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

/// execute triadic fn, recurse through arrays
pub(crate) fn exec3(f: Tri, a: &Value, b: &Value, c: &Value, m: bool) -> Result<Value, FnErr> {
	match (a, b, c) {
		(A(_), A(_), A(_)) |
		(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
		(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => { unsafe {	//iterate through array(s), bfs without recursion
			//NonNull pointers are required because of aliasing rules, soundness is ensured manually
			let mut az: Vec<Value> = Vec::new();	//resulting array
			let mut q: VecDeque<(&Value, &Value, &Value, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays
			q.push_back((a, b, c, (&mut az).into()));
			while let Some((a, b, c, mut dst)) = q.pop_front() {    //keep reading from front of queue
				match (a, b, c) {
					(A(aa), A(ab), A(ac)) => {	//current entry is three arrays
						if aa.len() == ab.len() && ab.len() == ac.len() {	//check lengths
							for ((va, vb), vc) in aa.iter().zip(ab).zip(ac) {
								match (va, vb, vc) {
									(A(_), A(_), A(_)) |
									(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
									(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
										dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
										let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
										q.push_back((va, vb, vc, ndst.into()));	//add next layer vecs to queue
									}
									(_, _, _) => {	//plain value, compute and store result
										dst.as_mut().push(f(va, vb, vc, m)?);
									}
								}
							}
						}
						else {
							return Err(Len3(aa.len(), ab.len(), ac.len()));
						}
					}
					(A(aa), A(ab), vc) => {	//current entry is array + array + value
						if aa.len() == ab.len() {	//check lengths
							for (va, vb) in aa.iter().zip(ab) {
								match (va, vb, vc) {
									(A(_), A(_), A(_)) |
									(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
									(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
										dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
										let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
										q.push_back((va, vb, vc, ndst.into()));	//add next layer vecs to queue
									}
									(_, _, _) => {	//plain value, compute and store result
										dst.as_mut().push(f(va, vb, vc, m)?);
									}
								}
							}
						}
						else {
							return Err(Len2(aa.len(), ab.len()));
						}
					}
					(A(aa), vb, A(ac)) => {	//current entry is array + value + array
						if aa.len() == ac.len() {
							for (va, vc) in aa.iter().zip(ac) {
								match (va, vb, vc) {
									(A(_), A(_), A(_)) |
									(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
									(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
										dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
										let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
										q.push_back((va, vb, vc, ndst.into()));	//add next layer vecs to queue
									}
									(_, _, _) => {	//plain value, compute and store result
										dst.as_mut().push(f(va, vb, vc, m)?);
									}
								}
							}
						}
						else {
							return Err(Len2(aa.len(), ac.len()));
						}
					}
					(va, A(ab), A(ac)) => {	//current entry is value + array + array
						if ab.len() == ac.len() {
							for (vb, vc) in ab.iter().zip(ac) {
								match (va, vb, vc) {
									(A(_), A(_), A(_)) |
									(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
									(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
										dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
										let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
										q.push_back((va, vb, vc, ndst.into()));	//add next layer vecs to queue
									}
									(_, _, _) => {	//plain value, compute and store result
										dst.as_mut().push(f(va, vb, vc, m)?);
									}
								}
							}
						}
						else {
							return Err(Len2(ab.len(), ac.len()));
						}
					}
					(A(aa), vb, vc) => {	//current entry is array + value + value
						for va in aa {
							match (va, vb, vc) {
								(A(_), A(_), A(_)) |
								(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
								(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
									dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
									let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
									q.push_back((va, vb, vc, ndst.into()));	//add next layer vecs to queue
								}
								(_, _, _) => {	//plain value, compute and store result
									dst.as_mut().push(f(va, vb, vc, m)?);
								}
							}
						}
					}
					(va, A(ab), vc) => {	//current entry is value + array + value
						for vb in ab {
							match (va, vb, vc) {
								(A(_), A(_), A(_)) |
								(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
								(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
									dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
									let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
									q.push_back((va, vb, vc, ndst.into()));	//add next layer vecs to queue
								}
								(_, _, _) => {	//plain value, compute and store result
									dst.as_mut().push(f(va, vb, vc, m)?);
								}
							}
						}
					}
					(va, vb, A(ac)) => {	//current entry is value + value + array
						for vc in ac {
							match (va, vb, vc) {
								(A(_), A(_), A(_)) |
								(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
								(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
									dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
									let A(ndst) = dst.as_mut().last_mut().unwrap() else { std::hint::unreachable_unchecked() };	//get pointer to destination
									q.push_back((va, vb, vc, ndst.into()));	//add next layer vecs to queue
								}
								(_, _, _) => {	//plain value, compute and store result
									dst.as_mut().push(f(va, vb, vc, m)?);
								}
							}
						}
					}
					(_, _, _) => {	//current entry is three values
						unreachable!();
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

	N(na), N(nb), _ => Ok(N(na + nb))	//add numbers
	
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
	
	N(na), N(nb), _ => Ok(N(na - nb))	//subtract numbers
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

	N(na), N(nb), _ => Ok(N(na * nb))	//multiply numbers
	
	S(sa), N(nb), _ => {	//repeat string
		let ib = r_i1(nb);
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
		let (fa, fb) = r_f2(na, nb);
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
	
	N(na), _ => f_r1(r_f1(na).ln()).map(N)	//natural log of number
	
	S(sa), false => Ok(N(sa.chars().count().into()))	//char length of string
	
	S(sa), true => Ok(N(sa.len().into()))	//byte length of string
);
