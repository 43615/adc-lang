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

pub enum Promote2<'a> {
	AA {
		aa: &'a [Value],
		ab: &'a [Value],
		i: usize
	},
	AS {
		aa: &'a [Value],
		vb: &'a Value,
		i: usize
	},
	SA {
		va: &'a Value,
		ab: &'a [Value],
		i: usize
	}
}
impl<'a> TryFrom<(&'a Value, &'a Value)> for Promote2<'a> {
	type Error = FnErr;

	fn try_from(value: (&'a Value, &'a Value)) -> Result<Self, Self::Error> {
		use Value::*;
		match value {
			(A(aa), A(ab)) => {
				if aa.len() != ab.len() { Err(Len2(aa.len(), ab.len())) }
				else { Ok(Self::AA { aa, ab, i: 0 }) }
			},
			(A(aa), vb) => Ok(Self::AS { aa, vb, i: 0 }),
			(va, A(ab)) => Ok(Self::SA { va, ab, i: 0 }),
			_ => unreachable!()
		}
	}
}
impl<'a> Iterator for Promote2<'a> {
	type Item = (&'a Value, &'a Value);

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::AA { aa, ab, i } => {
				let res = aa.get(*i).map(|va| (va, &ab[*i]));
				*i += 1;
				res
			},
			Self::AS { aa, vb, i } => {
				let res = aa.get(*i).map(|va| (va, &**vb));
				*i += 1;
				res
			},
			Self::SA { va, ab, i } => {
				let res = ab.get(*i).map(|vb| (&**va, vb));
				*i += 1;
				res
			},
		}
	}
}
impl std::iter::FusedIterator for Promote2<'_> {}

#[allow(clippy::upper_case_acronyms)]
pub enum Promote3<'a> {
	AAA {
		aa: &'a [Value],
		ab: &'a [Value],
		ac: &'a [Value],
		i: usize
	},
	AAS {
		aa: &'a [Value],
		ab: &'a [Value],
		vc: &'a Value,
		i: usize
	},
	ASA {
		aa: &'a [Value],
		vb: &'a Value,
		ac: &'a [Value],
		i: usize
	},
	SAA {
		va: &'a Value,
		ab: &'a [Value],
		ac: &'a [Value],
		i: usize
	},
	ASS {
		aa: &'a [Value],
		vb: &'a Value,
		vc: &'a Value,
		i: usize
	},
	SAS {
		va: &'a Value,
		ab: &'a [Value],
		vc: &'a Value,
		i: usize,
	},
	SSA {
		va: &'a Value,
		vb: &'a Value,
		ac: &'a [Value],
		i: usize
	}
}
impl<'a> TryFrom<(&'a Value, &'a Value, &'a Value)> for Promote3<'a> {
	type Error = FnErr;

	fn try_from(value: (&'a Value, &'a Value, &'a Value)) -> Result<Self, Self::Error> {
		use Value::*;
		match value {
			(A(aa), A(ab), A(ac)) => {
				if aa.len() != ab.len() || ab.len() != ac.len() { Err(Len3(aa.len(), ab.len(), ac.len())) }
				else { Ok(Self::AAA { aa, ab, ac, i: 0 }) }
			},
			(A(aa), A(ab), vc) => {
				if aa.len() != ab.len() { Err(Len2(aa.len(), ab.len())) }
				else { Ok(Self::AAS { aa, ab, vc, i: 0 }) }
			},
			(A(aa), vb, A(ac)) => {
				if aa.len() != ac.len() { Err(Len2(aa.len(), ac.len())) }
				else { Ok(Self::ASA { aa, vb, ac, i: 0 }) }
			},
			(va, A(ab), A(ac)) => {
				if ab.len() != ac.len() { Err(Len2(ab.len(), ac.len())) }
				else { Ok(Self::SAA { va, ab, ac, i: 0 }) }
			},
			(A(aa), vb, vc) => Ok(Self::ASS { aa, vb, vc, i: 0 }),
			(va, A(ab), vc) => Ok(Self::SAS { va, ab, vc, i: 0 }),
			(va, vb, A(ac)) => Ok(Self::SSA { va, vb, ac, i: 0 }),
			_ => unreachable!()
		}
	}
}
impl<'a> Iterator for Promote3<'a> {
	type Item = (&'a Value, &'a Value, &'a Value);

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::AAA { aa, ab, ac, i } => {
				let res = aa.get(*i).map(|va| (va, &ab[*i], &ac[*i]));
				*i += 1;
				res
			},
			Self::AAS { aa, ab, vc, i } => {
				let res = aa.get(*i).map(|va| (va, &ab[*i], &**vc));
				*i += 1;
				res
			},
			Self::ASA { aa, vb, ac, i } => {
				let res = ac.get(*i).map(|vc| (&aa[*i], &**vb, vc));
				*i += 1;
				res
			},
			Self::SAA { va, ab, ac, i } => {
				let res = ab.get(*i).map(|vb| (&**va, vb, &ac[*i]));
				*i += 1;
				res
			},
			Self::ASS { aa, vb, vc, i } => {
				let res = aa.get(*i).map(|va| (va, &**vb, &**vc));
				*i += 1;
				res
			},
			Self::SAS { va, ab, vc, i } => {
				let res = ab.get(*i).map(|vb| (&**va, vb, &**vc));
				*i += 1;
				res
			},
			Self::SSA { va, vb, ac, i } => {
				let res = ac.get(*i).map(|vc| (&**va, &**vb, vc));
				*i += 1;
				res
			},
		}
	}
}
impl std::iter::FusedIterator for Promote3<'_> {}