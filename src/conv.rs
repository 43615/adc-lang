//! Conversions of foreign types, here be opinions

use std::borrow::Cow;
use std::cmp::Ordering;
use bitvec::prelude::*;
use malachite::{Natural, Integer, Rational};
use malachite::base::num::basic::traits::{Zero, One};
use malachite::base::num::conversion::traits::RoundingFrom;
use malachite::base::rounding_modes::RoundingMode;
use crate::err::{FnErr::{self, *}, Utf8Err};
use crate::state::{Value::{self, *}, DEFAULT_PARAMS};

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
	use malachite::rational::rational::conversion::primitive_float_from_rational::FloatConversionError;
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

/// proper iterator over bytes from bitvec, right-aligned
pub fn bitvec_bytes(a: &BitVec<u8, Msb0>) -> impl DoubleEndedIterator<Item = u8> + ExactSizeIterator + std::iter::FusedIterator {
	a.rchunks(8).rev().map(|bs| bs.load_be::<u8>())
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
		match value {
			(A(aa), A(ab)) => {
				if aa.len() != ab.len() { Err(Len2(aa.len(), ab.len())) }
				else { Ok(Self::AA { aa, ab, i: 0 }) }
			},
			(A(aa), vb) => Ok(Self::AS { aa, vb, i: 0 }),
			(va, A(ab)) => Ok(Self::SA { va, ab, i: 0 }),
			_ => unsafe { std::hint::unreachable_unchecked() }
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
	fn size_hint(&self) -> (usize, Option<usize>) {
		let len = match self {
			Self::AA { aa, i, .. } => {aa.len().saturating_sub(*i)}
			Self::AS { aa, i, .. } => {aa.len().saturating_sub(*i)}
			Self::SA { ab, i, .. } => {ab.len().saturating_sub(*i)}
		};
		(len, Some(len))
	}
}
impl std::iter::FusedIterator for Promote2<'_> {}
impl ExactSizeIterator for Promote2<'_> {}

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
			_ => unsafe { std::hint::unreachable_unchecked() }
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
	fn size_hint(&self) -> (usize, Option<usize>) {
		let len = match self {
			Self::AAA { aa, i, .. } => {aa.len().saturating_sub(*i)}
			Self::AAS { aa, i, .. } => {aa.len().saturating_sub(*i)}
			Self::ASA { ac, i, .. } => {ac.len().saturating_sub(*i)}
			Self::SAA { ab, i, .. } => {ab.len().saturating_sub(*i)}
			Self::ASS { aa, i, .. } => {aa.len().saturating_sub(*i)}
			Self::SAS { ab, i, .. } => {ab.len().saturating_sub(*i)}
			Self::SSA { ac, i, .. } => {ac.len().saturating_sub(*i)}
		};
		(len, Some(len))
	}
}
impl std::iter::FusedIterator for Promote3<'_> {}
impl ExactSizeIterator for Promote3<'_> {}

/// Hybrid iterator over UTF-8 strings, may be advanced by bytes or chars. Does not expect valid UTF-8 internally.
///
/// Convertible [`From`] owned|borrowed × bytes|strings. Any manually set `pos` is handled safely.
///
/// [`Self::try_next_char`] parses a [`char`] at the current `pos` if it's valid UTF-8, and is used for [`TryInto<String>`].
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Utf8Iter<'a> {
	pub bytes: Cow<'a, [u8]>,
	pub pos: usize
}

impl From<Vec<u8>> for Utf8Iter<'_> {
	fn from(bytes: Vec<u8>) -> Self {
		Self { bytes: bytes.into(), pos: 0 }
	}
}
impl<'a> From<&'a [u8]> for Utf8Iter<'a> {
	fn from(bytes: &'a [u8]) -> Self {
		Self { bytes: bytes.into(), pos: 0 }
	}
}
impl From<String> for Utf8Iter<'_> {
	fn from(s: String) -> Self {
		Self { bytes: s.into_bytes().into(), pos: 0 }
	}
}
impl<'a> From<&'a str> for Utf8Iter<'a> {
	fn from(s: &'a str) -> Self {
		Self { bytes: s.as_bytes().into(), pos: 0 }
	}
}
impl TryFrom<Utf8Iter<'_>> for String {
	type Error = String;

	fn try_from(mut value: Utf8Iter) -> Result<String, String> {
		let mut res = String::new();
		while value.pos < value.bytes.len() {
			res.push(value.try_next_char().map_err(|e| format!("At byte {}: {e}", value.pos))?);
		}
		Ok(res)
	}
}

impl Iterator for Utf8Iter<'_> {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		self.bytes.get(self.pos).copied().inspect(|_| {self.pos+=1;})
	}

	/// Lower and upper bound on remaining encoded `char`s, assuming valid UTF-8.
	fn size_hint(&self) -> (usize, Option<usize>) {
		let len = self.bytes.len().saturating_sub(self.pos);
		(len.div_ceil(4), Some(len))
	}
}

impl Utf8Iter<'_> {
	/// Attempts to parse one UTF-8 character starting at the current `pos`, advancing it appropriately if successful.
	///
	/// Errors on invalid byte sequences and reports the erroneous values (in ADC literal format).
	///
	/// Handles all possible UTF-8 encoding errors:
	/// - Invalid bytes: C0, C1, F5–FF
	/// - Character starting with a continuation byte (80–BF)
	/// - Multi-byte character with missing / non-continuation byte(s)
	/// - Overlong encodings (3-byte below U+0800 or 4-byte below U+10000)
	/// - UTF-16 surrogates (U+D800–DFFF)
	/// - Values above U+10FFFF
	pub fn try_next_char(&mut self) -> Result<char, Utf8Err> {
		use Utf8Err::*;
		
		let Some(b0) = self.bytes.get(self.pos).copied() else {return Err(OutOfBounds(self.pos, self.bytes.len()));};
		let c;
		match b0 {
			//within ASCII
			0x00..=0x7F => {
				self.pos += 1;
				Ok(b0 as char)
			}

			//continuation byte
			0x80..=0xBF => {
				Err(ContAtStart(b0))
			}

			//2-byte char
			0xC2..=0xDF => {
				let Some(b1) = self.bytes.get(self.pos+1).copied() else {return Err(Missing1(b0));};
				if !(0x80..=0xBF).contains(&b1) {return Err(NonCont2(b0, b1));}
				c = (b0 as u32 & 0x1F) << 6 | (b1 as u32 & 0x3F);
				self.pos += 2;
				Ok(unsafe{char::from_u32_unchecked(c)})
			}

			//3-byte char
			0xE0..=0xEF => {
				let Some(b1) = self.bytes.get(self.pos+1).copied() else {return Err(Missing1(b0));};
				let Some(b2) = self.bytes.get(self.pos+2).copied() else {return Err(Missing2(b0, b1));};
				if !(0x80..=0xBF).contains(&b1) || !(0x80..=0xBF).contains(&b2) {return Err(NonCont3(b0, b1, b2));}
				c = (b0 as u32 & 0x0F) << 12 | (b1 as u32 & 0x3F) << 6 | (b2 as u32 & 0x3F);
				if c < 0x0800 {return Err(Overlong3(c, b0, b1, b2));}
				if (0xD800u32..=0xDFFFu32).contains(&c) {return Err(Utf16Surr(c, b0, b1, b2));}
				self.pos += 3;
				Ok(unsafe{char::from_u32_unchecked(c)})
			}

			//4-byte char
			0xF0..=0xF4 => {
				let Some(b1) = self.bytes.get(self.pos+1).copied() else {return Err(Missing1(b0));};
				let Some(b2) = self.bytes.get(self.pos+2).copied() else {return Err(Missing2(b0, b1));};
				let Some(b3) = self.bytes.get(self.pos+3).copied() else {return Err(Missing3(b0, b1, b2));};
				if !(0x80..=0xBF).contains(&b1) || !(0x80..=0xBF).contains(&b2) || !(0x80..=0xBF).contains(&b3) {return Err(NonCont4(b0, b1, b2, b3));}
				c = (b0 as u32 & 0x07) << 18 | (b1 as u32 & 0x3F) << 12 | (b2 as u32 & 0x3F) << 6 | (b3 as u32 & 0x3F);
				if c < 0x10000 {return Err(Overlong4(c, b0, b1, b2, b3));}
				if c > 0x10FFFF {return Err(TooLarge(c, b0, b1, b2, b3));}
				self.pos += 4;
				Ok(unsafe{char::from_u32_unchecked(c)})
			}

			0xC0 | 0xC1 | 0xF5..=0xFF => {
				Err(Impossible(b0))
			}
		}
	}

	/// Converts 1 or 2 [`Value`]s into a list of macros.
	///
	/// Nested array traversal using heap DFS with flattening. Value types should match the syntax in the ADC manual:
	/// - If only `top` is given, it must be a string.
	/// - If `top` and `sec` are given and `top` is a string, `sec` is not used and should be returned to the stack.
	/// - Otherwise, `sec` must be a string, and the number/boolean in `top` decides how often to run `sec`.
	/// - For arrays, the nesting layout and lengths must match exactly and the contained scalars must all be either strings or non-strings as above.
	///
	/// `Ok` contains the macros and repetition counts (omitting zero-count and no-op macros), and a flag indicating whether `sec` should be returned.
	pub fn try_macros(top: &Value, sec: Option<&Value>) -> Result<(Vec<(Self, Natural)>, bool), String> {
		use crate::err::TypeLabel;
		use crate::conv::Promote2;
		if let Some(sec) = sec {	//dyadic form
			if let Ok(res) = Self::try_macros(top, None) { return Ok(res); }	//see if monadic form succeeds (only strings in top), continue if not
			match (sec, top) {
				(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//traverse nested arrays
					let mut stk = vec![Promote2::try_from((sec, top)).map_err(|e| e.to_string())?];
					let mut res = Vec::new();

					while let Some(it) = stk.last_mut() {	//keep operating on the top iters
						if let Some((va, vb)) = it.next() {	//advance them
							match (va, vb) {
								(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//nested array(s)
									let nit = Promote2::try_from((va, vb)).map_err(|e| e.to_string())?;
									stk.push(nit);	//"recursive call"
								},
								(S(sa), B(bb)) => {
									let nb = Natural::from(bb.count_ones());
									let ui = Self::from(sa.to_owned());
									if nb != Natural::ZERO && !ui.is_finished_or_noop() {
										res.push((ui, nb));
									}
								},
								(S(sa), N(rb)) => {
									let Ok(nb) = Natural::try_from(rb)
										else {return Err(format!("Can't possibly repeat a macro {} times", crate::num::nauto(rb, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2)));};
									let ui = Self::from(sa.to_owned());
									if nb != Natural::ZERO && !ui.is_finished_or_noop() {
										res.push((ui, nb));
									}
								},
								(_, S(_)) => {	//legitimate strings are already covered by monadic form attempt
									return Err("Unexpected string in top array".into());
								},
								_ => {
									return Err(format!("Expected only pairs of macro strings and non-strings, found {} and {} in arrays", TypeLabel::from(sec), TypeLabel::from(top)));
								}
							}
						}
						else {	//if top iters have ended
							stk.pop();	//"return"
						}
					}

					Ok((res, false))
				},
				//scalars:
				(S(sa), B(bb)) => {
					let nb = Natural::from(bb.count_ones());
					let ui = Self::from(sa.to_owned());
					if nb != Natural::ZERO && !ui.is_finished_or_noop() {
						Ok((vec![(ui, nb)], false))
					}
					else {Ok((vec![], false))}
				},
				(S(sa), N(rb)) => {
					let Ok(nb) = Natural::try_from(rb)
						else {return Err(format!("Can't possibly repeat a macro {} times", crate::num::nauto(rb, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2)));};
					let ui = Self::from(sa.to_owned());
					if nb != Natural::ZERO && !ui.is_finished_or_noop() {
						Ok((vec![(ui, nb)], false))
					}
					else {Ok((vec![], false))}
				},
				(_, S(_)) => {	//already covered by monadic form attempt
					unsafe { std::hint::unreachable_unchecked() }
				},
				_ => {
					Err(format!("Expected a macro string and a non-string, {} and {} given", TypeLabel::from(sec), TypeLabel::from(top)))
				}
			}
		}
		else {	//monadic form
			match top {
				A(a) => {	//traverse nested array
					let mut stk: Vec<std::slice::Iter<Value>> = vec![a.iter()];
					let mut res = Vec::new();
					while let Some(i) = stk.last_mut() {	//keep operating on the top iter
						if let Some(val) = i.next() {	//advance it
							match val {
								A(na) => {	//nested array encountered
									stk.push(na.iter());	//"recursive call"
								},
								S(s) => {	//string encountered
									let ui = Self::from(s.to_owned());
									if !ui.is_finished_or_noop() {
										res.push((ui, Natural::ONE));
									}
								},
								_ => {
									return Err(format!("Expected only macro strings, found {} in array", TypeLabel::from(val)));
								}
							}
						}
						else {	//if top iter has ended
							stk.pop();	//"return"
						}
					}

					Ok((res, true))
				},
				//scalar:
				S(s) => {
					let ui = Self::from(s.to_owned());
					if !ui.is_finished_or_noop() {
						Ok((vec![(ui, Natural::ONE)], true))
					}
					else {Ok((vec![], true))}
				},
				_ => {
					Err(format!("Expected a macro string, {} given", TypeLabel::from(top)))
				}
			}
		}
	}

	pub(crate) fn is_finished_or_noop(&self) -> bool {
		let mut pos = self.pos;
		pos >= self.bytes.len()
		||
		loop {	//check if remainder is redundant
			if matches!(crate::byte_cmd(self.bytes[pos]), crate::Command::Space) {
				if self.bytes[pos] == b'#' {	//consume line comment
					while let Some(b) = self.bytes.get(pos).copied() && b != b'\n' {pos += 1;}
				}
			}
			else {break false;}	//non-whitespace found
			pos += 1;
			if pos >= self.bytes.len() {break true;}	//only whitespace found
		}
	}

	pub(crate) const fn back(&mut self) { self.pos -= 1; }

	pub(crate) const fn rewind(&mut self) { self.pos = 0; }
}