//! Nontrivial conversions and glue for foreign types

use std::cmp::Ordering;
use malachite::{Natural, Integer, Rational};
use malachite::base::num::conversion::traits::RoundingFrom;
use malachite::base::rounding_modes::RoundingMode;

use crate::errors::FnErr::{self, *};
use crate::structs::Value;

pub fn r_u(ra: &Rational) -> Result<usize, FnErr> {
	usize::try_from(ra).map_err(|_| Arith("Non-index given".into()))
}

pub fn r_n(ra: &Rational) -> Result<Natural, FnErr> {
	Natural::try_from(ra).map_err(|_| Arith("Non-natural given".into()))
}

pub fn r_i(ra: &Rational) -> Result<Integer, FnErr> {
	Integer::try_from(ra).map_err(|_| Arith("Non-integer given".into()))
}

pub fn r_f(ra: &Rational) -> Result<f64, FnErr> {
	use malachite::rational::conversion::primitive_float_from_rational::FloatConversionError;
	match f64::try_from(ra) {
		Ok(f) => {Ok(f)},
		Err(FloatConversionError::Inexact) => {Ok(f64::rounding_from(ra, RoundingMode::Nearest).0)},
		Err(FloatConversionError::Overflow) => {Err(Arith("Value is too large for 64-bit float".into()))},
		Err(FloatConversionError::Underflow) => {Err(Arith("Value is too small for 64-bit float".into()))}
	}
}

pub fn f_r(fa: f64) -> Result<Rational, FnErr> {
	if fa.is_nan() {Err(Arith("Floating-point result is NaN".into()))}
	else if fa == f64::INFINITY {Err(Arith("Floating-point result is +∞".into()))}
	else if fa == f64::NEG_INFINITY {Err(Arith("Floating-point result is -∞".into()))}
	else {
		Ok(Rational::try_from_float_simplest(fa).unwrap())
	}
}


/// shortlex comparison of strings
pub fn str_cmp(a: &str, b: &str) -> Ordering {
	let (mut a, mut b) = (a.chars(), b.chars());
	let mut o = Ordering::Equal;
	loop {
		match (a.next(), b.next()) {
			(Some(ca), Some(cb)) => { o = o.then(ca.cmp(&cb)); },	//find first difference
			(Some(_), None) => return Ordering::Greater,	//a is longer
			(None, Some(_)) => return Ordering::Less,		//b is longer
			(None, None) => return o						//same length, return first difference
		}
	}
}

/// Iterates through an array or promotes a plain value by repeating it endlessly. Needed for rank-polymorphy.
pub enum PromotingIter<'a> {
	Arr(&'a [Value], usize),
	Val(&'a Value)
}
impl<'a> From<&'a Value> for PromotingIter<'a> {
	fn from(value: &'a Value) -> Self {
		match value {
			Value::A(a) => Self::Arr(a, 0),
			_ => Self::Val(value)
		}
	}
}
impl<'a> Iterator for PromotingIter<'a> {
	type Item = &'a Value;
	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::Arr(arr, pos) => {
				let val = arr.get(*pos);
				*pos += 1;
				val
			},
			Self::Val(v) => {Some(v)}
		}
	}
}
impl std::iter::FusedIterator for PromotingIter<'_> {}

/// If both values are arrays, check if they have the same length
pub const fn lenck2(a: &Value, b: &Value) -> Result<(), FnErr> {
	use Value::*;
	if let (A(aa), A(ab)) = (a, b) { if aa.len() == ab.len() { Ok(()) } else { Err(Len2(aa.len(), ab.len())) } } else { Ok(()) }
}

/// If two or more values are arrays, check if they have the same length
pub const fn lenck3(a: &Value, b: &Value, c: &Value) -> Result<(), FnErr> {
	use Value::*;
	match (a, b, c) {
		(A(aa), A(ab), A(ac)) => if aa.len() == ab.len() && ab.len() == ac.len() { Ok(()) } else { Err(Len3(aa.len(), ab.len(), ac.len())) },
		(A(aa), A(ab), _) => { if aa.len() == ab.len() { Ok(()) } else { Err(Len2(aa.len(), ab.len())) } },
		(A(aa), _, A(ac)) => { if aa.len() == ac.len() { Ok(()) } else { Err(Len2(aa.len(), ac.len())) } },
		(_, A(ab), A(ac)) => { if ab.len() == ac.len() { Ok(()) } else { Err(Len2(ab.len(), ac.len())) } },
		_ => Ok(())
	}
}