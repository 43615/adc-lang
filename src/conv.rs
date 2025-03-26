//! Nontrivial conversions and glue

use std::iter::FusedIterator;
use malachite::{rational::Rational, Integer, Natural};
use malachite::base::num::arithmetic::traits::Abs;
use malachite::base::num::conversion::traits::{RoundingFrom, RoundingInto};
use malachite::base::rounding_modes::RoundingMode;
use crate::errors::FnErr::{self, *};
use crate::structs::Value;

#[inline(always)] pub(crate) fn r_i1(ra: &Rational) -> Integer {
	Integer::rounding_from(ra, RoundingMode::Down).0
}

#[inline(always)] pub(crate) fn r_i2(ra: &Rational, rb: &Rational) -> (Integer, Integer) {
	(r_i1(ra), r_i1(rb))
}

#[inline(always)] pub(crate) fn r_n1(ra: &Rational) -> Natural {
	Natural::rounding_from(ra.abs(), RoundingMode::Down).0
}

#[inline(always)] pub(crate) fn r_n2(ra: &Rational, rb: &Rational) -> (Natural, Natural) {
	(r_n1(ra), r_n1(rb))
}

#[inline(always)] pub(crate) fn r_n3(ra: &Rational, rb: &Rational, rc: &Rational) -> (Natural, Natural, Natural) {
	(r_n1(ra), r_n1(rb), r_n1(rc))
}

#[inline(always)] pub(crate) fn r_f1(ra: &Rational) -> f64 {
	f64::rounding_from(ra, RoundingMode::Nearest).0
}

#[inline(always)] pub(crate) fn r_f2(ra: &Rational, rb: &Rational) -> (f64, f64) {
	(r_f1(ra), r_f1(rb))
}

#[inline(always)] pub(crate) fn f_r1(fa: f64) -> Result<Rational, FnErr> {
	Rational::try_from(fa).map_err(|_| {
		if fa.is_nan() {Arith("result is NaN".into())}
		else if fa == f64::INFINITY {Arith("result is +∞".into())}
		else {Arith("result is -∞".into())}
	})
}

#[inline(always)] pub(crate) fn f_r2(fa: f64, fb: f64) -> Result<(Rational, Rational), FnErr> {
	let ra = f_r1(fa)?;
	let rb = f_r1(fb)?;
	Ok((ra, rb))
}

/// Iterates through an array or promotes a plain value by repeating it endlessly. Needed for rank-polymorphy.
pub(crate) enum PromotingIter<'a> {
	Arr(&'a Vec<Value>, usize),
	Val(&'a Value)
}
impl<'a> From<&'a Value> for PromotingIter<'a> {
	#[inline(always)] fn from(value: &'a Value) -> Self {
		match value {
			Value::A(a) => Self::Arr(a, 0),
			_ => Self::Val(value)
		}
	}
}
impl<'a> Iterator for PromotingIter<'a> {
	type Item = &'a Value;
	#[inline(always)] fn next(&mut self) -> Option<Self::Item> {
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
impl FusedIterator for PromotingIter<'_> {}

/// If both values are arrays, check if they have the same length
#[inline(always)] pub(crate) fn lenck2(a: &Value, b: &Value) -> Result<(), FnErr> {
	use Value::*;
	if let (A(aa), A(ab)) = (a, b) { if aa.len() == ab.len() { Ok(()) } else { Err(Len2(aa.len(), ab.len())) } } else { Ok(()) }
}

/// If two or more values are arrays, check if they have the same length
#[inline(always)] pub(crate) fn lenck3(a: &Value, b: &Value, c: &Value) -> Result<(), FnErr> {
	use Value::*;
	match (a, b, c) {
		(A(aa), A(ab), A(ac)) => if aa.len() == ab.len() && ab.len() == ac.len() { Ok(()) } else { Err(Len3(aa.len(), ab.len(), ac.len())) },
		(A(aa), A(ab), _) => { if aa.len() == ab.len() { Ok(()) } else { Err(Len2(aa.len(), ab.len())) } },
		(A(aa), _, A(ac)) => { if aa.len() == ac.len() { Ok(()) } else { Err(Len2(aa.len(), ac.len())) } },
		(_, A(ab), A(ac)) => { if ab.len() == ac.len() { Ok(()) } else { Err(Len2(ab.len(), ac.len())) } },
		_ => Ok(())
	}
}