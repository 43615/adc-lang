//! Conversion functions

use malachite::{Rational, Integer, Natural};
use malachite::num::conversion::traits::{RoundingFrom, RoundingInto};
use malachite::rounding_modes::RoundingMode;
use crate::errors::FnErr::{self, *};

#[inline(always)] pub(crate) fn rti1(ra: &Rational) -> Integer {
	Integer::rounding_from(ra, RoundingMode::Down).0
}

#[inline(always)] pub(crate) fn rti2(ra: &Rational, rb: &Rational) -> (Integer, Integer) {
	(rti1(ra), rti1(rb))
}

#[inline(always)] pub(crate) fn rtf1(ra: &Rational) -> f64 {
	f64::rounding_from(ra, RoundingMode::Nearest).0
}

#[inline(always)] pub(crate) fn rtf2(ra: &Rational, rb: &Rational) -> (f64, f64) {
	(rtf1(ra), rtf1(rb))
}

#[inline(always)] pub(crate) fn ftr1(fa: f64) -> Result<Rational, FnErr> {
	Rational::try_from(fa).map_err(|_| {
		if fa.is_nan() {Arith("NaN".into())}
		else if fa == f64::INFINITY {Arith("+∞".into())}
		else {Arith("-∞".into())}
	})
}

#[inline(always)] pub(crate) fn ftr2(fa: f64, fb: f64) -> Result<(Rational, Rational), FnErr> {
	let ra = ftr1(fa)?;
	let rb = ftr1(fb)?;
	Ok((ra, rb))
}