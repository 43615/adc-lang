//! Pure functions and polymorphic execution engine
//! 
//! Functions have 1, 2, or 3 `&Value` parameters (monadic, dyadic, triadic) and a boolean mode switch (false by default, enabled by \` command)

#![allow(dead_code)]	//TODO: make not dead

use malachite::{Integer, Rational};
use malachite::num::arithmetic::traits::{Pow, Reciprocal};
use malachite::num::conversion::traits::{RoundingFrom, RoundingInto};
use regex::Regex;
use crate::conv::rtf2;
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


/// execute monadic fn, automatic iteration
pub(crate) fn exec1(f: Mon, mut a: &Value) -> Result<Value, FnErr> {
	
	todo!()
}

/// execute dyadic fn, automatic iteration
pub(crate) fn exec2(f: Dya, mut a: &Value, mut b: &Value) -> Result<Value, FnErr> {
	
	todo!()
}

dya!(add
	B(ba), B(bb), _ => Ok(B(*ba || *bb))	//boolean or
	
	N(na), N(nb), _ => Ok(N(na + nb))	//add numbers
	
	S(sa), S(sb), _ => Ok(S(sa.to_owned() + sb))	//concat strings
	
	AB(aba), AB(abb), _ => {	//boolean or (packed)
		if aba.len() == abb.len() {
			let mut res = aba.to_owned();
			res.or(abb);
			Ok(AB(res))
		}
		else {
			Err(Len(aba.len(), abb.len()))
		}
	}
);

dya!(sub
	N(na), N(nb), _ => Ok(N(na - nb))	//subtract numbers
);

dya!(mul
	B(ba), B(bb), _ => Ok(B(*ba && *bb))	//boolean and

	N(na), N(nb), _ => Ok(N(na * nb))	//multiply numbers
	
	S(sa), N(nb), _ => {	//repeat string
		let ib = rti1(nb);
		match usize::try_from(&ib) {
			Ok(ub) if sa.len().checked_mul(ub).is_some() => Ok(S(sa.repeat(ub))),
			_ => Err(Index(ib.to_owned()))
		}
	}
	
	AB(aba), AB(abb), _ => {	//boolean and (packed)
		if aba.len() == abb.len() {
			let mut res = aba.to_owned();
			res.and(abb);
			Ok(AB(res))
		}
		else {
			Err(Len(aba.len(), abb.len()))
		}
	}
);

dya!(div
	N(na), N(ba), _ => Ok(N(na / ba))	//divide numbers
);

mon!(inv	
	B(ba), _ => Ok(B(!ba))	//negate boolean
	
	N(na), _ => Ok(N(na.reciprocal()))	//reciprocate number
	
	S(sa), _ => Ok(S(sa.chars().rev().collect()))	//reverse string
	
	AB(aba), _ => Ok(AB({let mut res = aba.to_owned(); res.negate(); res}))	//negate packed booleans
);

dya!(pow
	B(ba), B(bb), _ => Ok(B(ba != bb))	//boolean xor
	
	N(na), N(nb), _ => {	//raise number to power
		let (fa, fb) = rtf2(na, nb);
		ftr1(fa.powf(fb)).map(N)
	}
	
	S(sa), S(sb), false => {	//find substring by literal
		if let Some(bidx) = sa.find(sb) {	//byte position
			Ok(N(sa.char_indices().position(|(cidx, _)| cidx==bidx).unwrap().into()))	//char position
		}
		else {
			Ok(N(Rational::from(-1_i8)))
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
			Ok(N(Rational::from(-1_i8)))
		}
	}
	
	AB(aba), AB(abb), _ => {	//boolean xor (packed)
		if aba.len() == abb.len() {
			let mut res = aba.to_owned();
			res.xor(abb);
			Ok(AB(res))
		}
		else {
			Err(Len(aba.len(), abb.len()))
		}
	}
);
