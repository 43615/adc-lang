//! Nontrivial conversion functions

use malachite::{Rational, Integer, Natural};
use malachite::num::conversion::traits::{RoundingFrom, RoundingInto};
use malachite::rounding_modes::RoundingMode;
use crate::errors::FnErr::{self, *};

#[inline(always)] pub(crate) fn r_i1(ra: &Rational) -> Integer {
	Integer::rounding_from(ra, RoundingMode::Down).0
}

#[inline(always)] pub(crate) fn r_i2(ra: &Rational, rb: &Rational) -> (Integer, Integer) {
	(r_i1(ra), r_i1(rb))
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