//! Storage structs and methods

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, RwLock, mpsc::Sender};
use bitvec::prelude::*;
use malachite::{Natural, Rational};
use malachite::base::num::basic::traits::{Zero, One};
use regex::{Regex, RegexBuilder};

#[derive(Debug)]
pub enum Value {
	B(BitVec<usize, Lsb0>),
	N(Rational),
	S(String),
	A(Vec<Value>)
}
impl Default for Value {
	fn default() -> Self {
		Self::A(Vec::new())
	}
}

/// Non-recursive heap DFS to avoid overflowing the stack.
impl Drop for Value {
	fn drop(&mut self) {
		use Value::*;
		use std::mem::take;
		if let A(a) = self {
			let mut q: Vec<Vec<Value>> = vec![take(a)];	//init queue for arrays to be processed
			while let Some(mut a) = q.pop() {
				for v in &mut a {	//traverse current array
					if let A(aa) = v {	//if a nested array is encountered
						q.push(take(aa))	//move it to the queue
					}
				}
				//current array drops here, with no nested contents
			}
		}
		//scalar values drop themselves after this
	}
}

/// Non-recursive heap BFS to avoid overflowing the stack, using a monadic identity.
impl Clone for Value {
	fn clone(&self) -> Self {
		use Value::*;
		match self {
			//scalar base cases
			B(b) => B(b.clone()),
			N(n) => N(n.clone()),
			S(s) => S(s.clone()),
			//traverse array using heap pseudorecursion, perform (cloning) identity function on every value
			//`exec1` takes over instead of continuing call recursion, `f` is never called on `A`.
			A(_) => unsafe { crate::fns::exec1(|v, _| Ok(v.clone()), self, false).unwrap_unchecked() }	//SAFETY: clone is infallible
		}
	}
}

impl Value {
	/// Prints scalar values, passing `A` is undefined behavior. `sb` prints brackets around strings.
	unsafe fn display_scalar(&self, k: usize, o: &Natural, nm: NumOutMode, sb: bool) -> String {
		use Value::*;
		match self {
			B(b) => {
				b.iter().by_vals().map(|b| if b {'T'} else {'F'}).collect()
			},
			N(n) => {
				use crate::num::*;
				use NumOutMode::*;
				match nm {
					Auto => {
						nauto(n, k, o)
					},
					Norm => {
						let (neg, ipart, fpart, rpart) = digits(n, k, o);
						nnorm(neg, &ipart, &fpart, &rpart, o)
					},
					Sci => {
						let (neg, ipart, fpart, rpart) = digits(n, k, o);
						nsci(neg, &ipart, &fpart, &rpart, o)
					},
					Frac => {
						nfrac(n, k, o)
					},
				}
			},
			S(s) => {
				if sb {
					String::from("[") + s + "]"
				}
				else {
					s.to_owned()
				}
			},
			A(_) => {
				unsafe { std::hint::unreachable_unchecked() }
			}
		}
	}
	
	/// Prints the value, heap DFS to avoid overflowing the stack. `sb` prints brackets around strings.
	pub fn display(&self, k: usize, o: &Natural, nm: NumOutMode, sb: bool) -> String {
		use Value::*;
		match self {
			A(a) => {	//the fun part
				let mut stk: Vec<std::slice::Iter<Value>> = vec![a.iter()];
				let mut res = String::from('(');
				while let Some(i) = stk.last_mut() {	//keep operating on the top iter
					if let Some(val) = i.next() {	//advance it
						match val {
							A(aa) => {	//nested array encountered
								if res.ends_with(' ') {res.pop();}
								res.push('(');
								stk.push(aa.iter());	//"recursive call"
							},
							_ => {	//scalar encountered
								let s = unsafe { val.display_scalar(k, o, nm, sb) };
								res += &s;	//append it
								res.push(' ');
							}
						}
					}
					else {	//if top iter has ended
						if res.ends_with(' ') {
							res.pop();	//remove trailing space
						}
						res.push(')');
						stk.pop();	//"return"
					}
				}
				res
			},
			_ => {	//just display scalar
				unsafe { self.display_scalar(k, o, nm, sb) }
			}
		}
	}
}

/// Canonical printing function, with default parameters for `N` and brackets for `S`.
impl Display for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.display(DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2, DEFAULT_PARAMS.3, true))
	}
}

/// Tries to convert register index ([`Rational`]) to a string if it was likely set from one, or displays the number with automatic format
pub fn reg_index_nice(ri: &Rational) -> String {
	use malachite::base::num::conversion::traits::PowerOf2Digits;
	Natural::try_from(ri).ok().and_then(|n| {	//if ri is a natural
		let bytes: Vec<u8> = n.to_power_of_2_digits_desc(8);	//look at its bytes
		str::from_utf8(&bytes).ok().map(|s| String::from("[") + s + "]")	//if it parses to a string, ri was likely set from one
	})
		.unwrap_or_else(|| {
			crate::num::nauto(ri, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2)	//or just return the number in canonical notation
		})
}

pub type ThreadResult = (Vec<Arc<Value>>, std::io::Result<crate::ExecResult>);

#[derive(Default, Debug)]
pub struct Register {
	/// Stack of values, [`Arc`] to allow shallow copies
	pub v: Vec<Arc<Value>>,
	/// Thread handle with result values, kill signal
	pub th: Option<(std::thread::JoinHandle<ThreadResult>, Sender<()>)>
}
/// Clone without thread handles since those are unique
impl Clone for Register {
	fn clone(&self) -> Self {
		Self {
			v: self.v.clone(),
			th: None
		}
	}
}

/// Register storage with different indexing modes
#[derive(Clone, Debug)]
pub struct RegStore {
	/// this covers ASCII integer values for frequently-accessed registers (commands like `sa`)
	pub low: [Register; 128],

	/// arbitrary rational indices otherwise
	pub high: HashMap<Rational, Register>
}
impl RegStore {
	/// Get register reference, don't create a register if not present
	pub fn try_get(&self, index: &Rational) -> Option<&Register> {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get(u))
			.or_else(|| self.high.get(index))
	}

	/// Get mutable register reference, don't create a register if not present
	pub fn try_get_mut(&mut self, index: &Rational) -> Option<&mut Register> {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get_mut(u))
			.or_else(|| self.high.get_mut(index))
	}

	/// Get mutable register reference, create register if necessary
	pub fn get_mut(&mut self, index: &Rational) -> &mut Register {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get_mut(u))
			.unwrap_or_else(|| self.high.entry(index.clone()).or_default())
	}

	/// Discard unused registers, reallocate used ones to fit
	pub fn trim(&mut self) {
		for reg in &mut self.low {
			reg.v.shrink_to_fit();
		}
		self.high.retain(|_, reg| {
			if reg.v.is_empty() {
				reg.th.is_some()
			}
			else {
				reg.v.shrink_to_fit();
				true
			}
		});
	}

	/// Clear all values, don't touch thread handles in `self`
	pub fn clear_vals(&mut self) {
		for reg in &mut self.low {
			reg.v = Vec::new();
		}
		self.high.retain(|_, reg| {
			reg.v = Vec::new();
			reg.th.is_some()
		});
		self.high.shrink_to_fit();
	}

	/// Replace values with those from `other`, keep thread handles in `self` and discard any in `other`
	pub fn replace_vals(&mut self, mut other: Self) {
		for (reg, oreg) in self.low.iter_mut().zip(other.low.into_iter()) {
			reg.v = oreg.v;
		}
		self.high.retain(|ri, reg| {	//retain existing regs if:
			if let Some(oreg) = other.high.remove(ri) && !oreg.v.is_empty() {	//other has one with the same index
				reg.v = oreg.v;
				true
			}
			else {	//or a thread handle
				reg.v = Vec::new();
				reg.th.is_some()
			}
		});
		for (ori, oreg) in other.high.drain().filter_map(|(ori, oreg)| (!oreg.v.is_empty()).then_some((ori, Register {v: oreg.v, th: None}))) {
			self.high.insert(ori, oreg);
		}
	}

	/// Joins all thread handles, `kill = true` sends kill signals.
	/// 
	/// Returns messages from any unhappy terminations, same as the `j` command.
	pub fn end_threads(&mut self, kill: bool) -> Vec<String> {
		use crate::ExecResult::*;
		
		let mut res = Vec::new();
		
		for (ri, reg) in
			self.low.iter_mut().enumerate().map(|(ri, reg)| (Rational::from(ri), reg))
				.chain(self.high.iter_mut().map(|(ri, reg)| (ri.clone(), reg)))
		{
			if let Some((jh, tx)) = reg.th.take() {
				if kill {
					tx.send(()).unwrap_or_else(|_| panic!("Thread {} panicked, terminating!", reg_index_nice(&ri)));
				}
				match jh.join() {
					Ok(mut tr) => {
						match tr.1 {
							Err(e) => {
								res.push(format!("IO error in thread {}: {}", reg_index_nice(&ri), e));
							},
							Ok(SoftQuit(c)) if c != 0 => {
								res.push(format!("Thread {} quit with code {}", reg_index_nice(&ri), c));
							},
							Ok(HardQuit(c)) if c != 0 => {
								res.push(format!("Thread {} hard-quit with code {}", reg_index_nice(&ri), c));
							},
							Ok(Killed) => {
								res.push(format!("Thread {} was killed", reg_index_nice(&ri)));
							},
							_ => {}
						}
						
						reg.v.append(&mut tr.0);
					},
					Err(e) => {
						std::panic::resume_unwind(e);
					}
				}
			}
		}
		res
	}
}
impl Default for RegStore {
	fn default() -> Self {
		Self {
			low: std::array::from_fn(|_| Register::default()),
			high: HashMap::default()
		}
	}
}

/// globally shareable cache for compiled regex automata
#[derive(Default, Debug)]
#[repr(transparent)] pub(crate) struct RegexCache(pub(crate) RwLock<HashMap<String, Regex>>);

impl RegexCache {
	/// get regex from cache or freshly compile
	pub(crate) fn get(&self, s: &String) -> Result<Regex, String> {
		if let Some(re) = self.0.read().unwrap().get(s) {
			Ok(re.clone())
		}
		else {
			match RegexBuilder::new(s)
				.size_limit(usize::MAX)
				.dfa_size_limit(usize::MAX)
				.build() {
				Ok(re) => {
					self.0.write().unwrap().insert(s.clone(), re.clone());
					Ok(re)
				},
				Err(e) => Err(format!("can't compile regex: {e}"))
			}
		}
	}

	pub(crate) fn clear(&self) {
		*self.0.write().unwrap() = HashMap::new();
	}
}

/// Hybrid iterator over UTF-8 strings, may be advanced by bytes or chars. Does not expect valid UTF-8 internally.
///
/// Convertible [`From`] owned|borrowed × bytes|strings. Manually set nonzero positions are handled as expected.
///
/// [`Self::try_next_char`] parses a [`char`] if it's valid UTF-8, and is used for [`TryInto<String>`].
#[derive(Clone, Debug)]
pub enum Utf8Iter<'a> {
	Owned {
		bytes: Vec<u8>,
		pos: usize
	},
	Borrowed {
		bytes: &'a [u8],
		pos: usize
	}
}
impl Default for Utf8Iter<'_> {
	fn default() -> Self {
		Self::Owned { bytes: Vec::new(), pos: 0 }
	}
}

impl From<Vec<u8>> for Utf8Iter<'_> {
	fn from(bytes: Vec<u8>) -> Self {
		Self::Owned { bytes, pos: 0 }
	}
}
impl<'a> From<&'a [u8]> for Utf8Iter<'a> {
	fn from(bytes: &'a [u8]) -> Self {
		Self::Borrowed { bytes, pos: 0 }
	}
}
impl From<String> for Utf8Iter<'_> {
	fn from(s: String) -> Self {
		Self::Owned { bytes: s.into_bytes(), pos: 0 }
	}
}
impl<'a> From<&'a str> for Utf8Iter<'a> {
	fn from(s: &'a str) -> Self {
		Self::Borrowed { bytes: s.as_bytes(), pos: 0 }
	}
}
impl TryFrom<Utf8Iter<'_>> for String {
	type Error = String;

	fn try_from(mut value: Utf8Iter) -> Result<String, String> {
		let mut res = String::new();
		while !value.is_finished() {
			res.push(value.try_next_char().map_err(|e| format!("At byte {}: {e}", {
				match value {
					Utf8Iter::Borrowed {pos, ..} => pos,
					Utf8Iter::Owned {pos, ..} => pos
				}
			}))?);
		}
		Ok(res)
	}
}

impl Iterator for Utf8Iter<'_> {
	type Item = u8;
	fn next(&mut self) -> Option<Self::Item> {
		let (bytes, pos): (&[u8], &mut usize) = match self {
			Self::Borrowed {bytes, pos} => (bytes, pos),
			Self::Owned {bytes, pos} => (bytes, pos)
		};
		bytes.get(*pos).copied().inspect(|_| {*pos+=1;})
	}
}

impl Utf8Iter<'_> {
	/// Parses one UTF-8 character starting at the current position, advancing the position in-place.
	///
	/// Errors on invalid byte sequences and reports the erroneous values (in ADC literal format), `pos` only advances if parsing is successful.
	///
	/// Handles all possible UTF-8 encoding errors:
	/// - Invalid bytes: C0, C1, F5–FF
	/// - Character starting with a continuation byte (80–BF)
	/// - Multi-byte character with missing / non-continuation byte(s)
	/// - Overlong encodings (3-byte below U+0800 or 4-byte below U+10000)
	/// - UTF-16 surrogates (U+D800–DFFF)
	/// - Values above U+10FFFF
	pub fn try_next_char(&mut self) -> Result<char, String> {
		let (bytes, pos): (&[u8], &mut usize) = match self {
			Self::Borrowed {bytes, pos} => (bytes, pos),
			Self::Owned {bytes, pos} => (bytes, pos)
		};
		if let Some(b0) = bytes.get(*pos).copied() {
			let c;
			match b0 {
				//within ASCII
				0x00..=0x7F => {
					*pos += 1;
					Ok(b0 as char)
				}

				//continuation byte
				0x80..=0xBF => {
					Err(format!("Continuation byte at start of character: [\\{b0:02X}]"))
				}

				//2-byte char
				0xC2..=0xDF => {
					if let Some(b1) = bytes.get(*pos+1).copied() {
						if !(0x80..=0xBF).contains(&b1) {
							return Err(format!("Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}]"));
						}
						c = ((b0 & 0x1F) as u32) << 6
							| (b1 & 0x3F) as u32;
						//all good:
						*pos += 2;
						Ok(unsafe{char::from_u32_unchecked(c)})
					}
					else {
						Err(format!("Unexpected end of string: [\\{b0:02X}]"))
					}
				}

				//3-byte char
				0xE0..=0xEF => {
					if let Some(b1) = bytes.get(*pos+1).copied() {
						if let Some(b2) = bytes.get(*pos+2).copied() {
							if !(0x80..=0xBF).contains(&b1) {
								return Err(format!("Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]"));
							}
							if !(0x80..=0xBF).contains(&b2) {
								return Err(format!("Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]"));
							}
							c = ((b0 & 0x0F) as u32) << 12
								| ((b1 & 0x3F) as u32) << 6
								| (b2 & 0x3F) as u32;
							if c < 0x0800 {
								return Err(format!("Overlong encoding of U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]"));
							}
							if (0xD800u32..=0xDFFFu32).contains(&c) {
								return Err(format!("Unexpected UTF-16 surrogate U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]"));
							}
							//all good:
							*pos += 3;
							Ok(unsafe{char::from_u32_unchecked(c)})
						}
						else {
							Err(format!("Unexpected end of string: [\\{b0:02X}\\{b1:02X}]"))
						}
					}
					else {
						Err(format!("Unexpected end of string: [\\{b0:02X}]"))
					}
				}

				//4-byte char
				0xF0..=0xF4 => {
					if let Some(b1) = bytes.get(*pos+1).copied() {
						if let Some(b2) = bytes.get(*pos+2).copied() {
							if let Some(b3) = bytes.get(*pos+3).copied() {
								if !(0x80..=0xBF).contains(&b1) {
									return Err(format!("Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]"));
								}
								if !(0x80..=0xBF).contains(&b2) {
									return Err(format!("Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]"));
								}
								if !(0x80..=0xBF).contains(&b3) {
									return Err(format!("Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]"));
								}
								c = ((b0 & 0x07) as u32) << 18
									| ((b1 & 0x3F) as u32) << 12
									| ((b2 & 0x3F) as u32) << 6
									| (b3 & 0x3F) as u32;
								if c < 0x10000 {
									return Err(format!("Overlong encoding of U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]"));
								}
								if c > 0x10FFFF {
									return Err(format!("Out-of-range character U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]"));
								}
								//all good:
								*pos += 4;
								Ok(unsafe{char::from_u32_unchecked(c)})
							}
							else {
								Err(format!("Unexpected end of string: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]"))
							}
						}
						else {
							Err(format!("Unexpected end of string: [\\{b0:02X}\\{b1:02X}]"))
						}
					}
					else {
						Err(format!("Unexpected end of string: [\\{b0:02X}]"))
					}
				}

				0xC0 | 0xC1 | 0xF5..=0xFF => Err(format!("Impossible byte in string: [\\{b0:02X}]")),
			}
		}
		else { 
			Err(format!("Position out of bounds: {}", *pos))
		}
	}

	/// Converts 1 or 2 [`Value`]s into a stack of macros.
	///
	/// Nested array traversal using heap DFS with flattening. Value types should match the syntax in the ADC manual:
	/// - If only `top` is given, it must be a string.
	/// - If `top` and `sec` are given and `top` is a string, `sec` is not used and should be returned to the stack.
	/// - Otherwise, `sec` must be a string, and the number/boolean in `top` decides how often to run `sec`.
	/// - For arrays, the nesting layout and lengths must match exactly and the contained scalars must all be either strings or non-strings as above.
	///
	/// `Ok` contains the macros and repetition counts in reverse (call stack) order and a flag indicating whether `sec` should be returned.
	pub fn from_vals(top: &Value, sec: Option<&Value>) -> Result<(Vec<(Self, Natural)>, bool), String> {
		use Value::*;
		use crate::errors::TypeLabel;
		use crate::conv::{PromotingIter, lenck2};
		if let Some(sec) = sec {	//dyadic form
			if let Ok(res) = Self::from_vals(top, None) { return Ok(res); }	//see if monadic form succeeds (only strings in top), continue if not
			match (sec, top) {
				(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//traverse nested arrays
					if let Err(e) = lenck2(sec, top) { return Err(e.to_string()); }
					let mut stk = vec![(PromotingIter::from(sec), PromotingIter::from(top))];
					let mut res = vec![];

					while let Some((ia, ib)) = stk.last_mut() {	//keep operating on the top iters
						if let (Some(va), Some(vb)) = (ia.next(), ib.next()) {	//advance them
							match (va, vb) {
								(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//nested array(s)
									if let Err(e) = lenck2(va, vb) { return Err(e.to_string()); }
									stk.push((PromotingIter::from(va), PromotingIter::from(vb)));	//"recursive call"
								},
								(S(sa), B(bb)) => {
									let nb = Natural::from(bb.count_ones());
									if nb != Natural::ZERO {
										res.push((sa.to_owned().into(), nb));
									}
								},
								(S(sa), N(rb)) => {
									if let Ok(nb) = Natural::try_from(rb) {
										if nb != Natural::ZERO {
											res.push((sa.to_owned().into(), nb));
										}
									}
									else {
										return Err(format!("Can't possibly repeat a macro {} times", crate::num::nauto(rb, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2)));
									}
								},
								(_, S(_)) => {	//legitimate strings are already covered by monadic form attempt
									return Err("Unexpected string in top array".into());
								},
								_ => {
									return Err(format!("Expected only matching strings and non-strings, found {} and {} in arrays", TypeLabel::from(sec), TypeLabel::from(top)));
								}
							}
						}
						else {	//if top iters have ended
							stk.pop();	//"return"
						}
					}

					res.reverse();
					Ok((res, false))
				},
				//scalars:
				(S(sa), B(bb)) => {
					let nb = Natural::from(bb.count_ones());
					if nb != Natural::ZERO {
						Ok((vec![(sa.to_owned().into(), nb)], false))
					}
					else {Ok((vec![], false))}
				},
				(S(sa), N(rb)) => {
					if let Ok(nb) = Natural::try_from(rb) {
						if nb != Natural::ZERO {
							Ok((vec![(sa.to_owned().into(), nb)], false))
						}
						else {Ok((vec![], false))}
					}
					else {
						Err(format!("Can't possibly repeat a macro {} times", crate::num::nauto(rb, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2)))
					}
				},
				(_, S(_)) => {	//already covered by monadic form attempt
					unsafe { std::hint::unreachable_unchecked() }
				},
				_ => {
					Err(format!("Expected a string and a non-string, {} and {} given", TypeLabel::from(sec), TypeLabel::from(top)))
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
									res.push((s.to_owned().into(), Natural::ONE));
								},
								_ => {
									return Err(format!("Expected only strings, found {} in array", TypeLabel::from(val)));
								}
							}
						}
						else {	//if top iter has ended
							stk.pop();	//"return"
						}
					}
					res.reverse();
					Ok((res, true))
				},
				//scalar:
				S(s) => {
					Ok((vec![(s.to_owned().into(), Natural::ONE)], true))
				},
				_ => {
					Err(format!("Expected a string, {} given", TypeLabel::from(top)))
				}
			}
		}
	}
	
	pub(crate) fn is_finished(&self) -> bool {
		let (bytes, pos): (&[u8], &usize) = match self {
			Self::Borrowed {bytes, pos} => (bytes, pos),
			Self::Owned {bytes, pos} => (bytes, pos)
		};
		bytes.len() <= *pos
	}
	
	pub(crate) fn back(&mut self) {
		let pos = match self {
			Self::Borrowed {pos, ..} => pos,
			Self::Owned {pos, ..} => pos
		};
		*pos -= 1;
	}

	pub(crate) fn rewind(&mut self) {
		let pos = match self {
			Self::Borrowed {pos, ..} => pos,
			Self::Owned {pos, ..} => pos
		};
		*pos = 0;
	}
}

/// Number output mode
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)] pub enum NumOutMode { #[default] Auto=0, Norm=1, Sci=2, Frac=3 }

/// Parameter context tuple: (K, I, O, M)
pub type Params = (usize, Natural, Natural, NumOutMode);

/// Default values (0, 10, 10, auto)
pub const DEFAULT_PARAMS: Params = {
	(0, Natural::const_from(10), Natural::const_from(10), NumOutMode::Auto)
};

/// Stack for numeric IO parameter contexts, with checked accessors
#[derive(Clone, Debug)]
#[repr(transparent)] pub struct ParamStk(Vec<Params>);
impl ParamStk {
	/// Creates new context with [`DEFAULT_PARAMS`]
	pub fn create(&mut self) {
		self.0.push(DEFAULT_PARAMS)
	}

	/// Returns to previous context, creates default if none left
	pub fn destroy(&mut self) {
		self.0.pop();
		if self.0.is_empty() {self.create();}
	}

	/// Discards all contexts, creates default
	pub fn clear(&mut self) {
		*self = Self::default();
	}

	/// Refers to the underlying [`Vec`] for manual access
	pub fn inner(&self) -> &Vec<Params> {
		&self.0
	}

	/// Extracts the underlying [`Vec`] for manual access
	pub fn into_inner(self) -> Vec<Params> {
		self.0
	}

	/// Checked edit of current output precision
	pub fn try_set_k(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(u) = r.try_into() {
			unsafe { self.0.last_mut().unwrap_unchecked().0 = u; }
			Ok(())
		}
		else {Err(const_format::concatcp!("Output precision must be a natural number <={}", usize::MAX))}
	}

	/// Checked edit of current input base
	pub fn try_set_i(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=2u8 {
			unsafe { self.0.last_mut().unwrap_unchecked().1 = n; }
			Ok(())
		}
		else {Err("Input base must be a natural number >=2")}
	}

	/// Checked edit of current output base
	pub fn try_set_o(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=2u8 {
			unsafe { self.0.last_mut().unwrap_unchecked().2 = n; }
			Ok(())
		}
		else {Err("Output base must be a natural number >=2")}
	}

	/// Checked edit of current output mode
	pub fn try_set_m(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(u) = u8::try_from(r) && u<=3 {
			unsafe { self.0.last_mut().unwrap_unchecked().3 = std::mem::transmute::<u8, NumOutMode>(u); }
			Ok(())
		}
		else {Err("Output mode must be 0|1|2|3")}
	}
	
	/// Infallible edit of current output precision
	pub fn set_k(&mut self, u: usize) {
		unsafe { self.0.last_mut().unwrap_unchecked().0 = u; }
	}

	/// Infallible edit of current output mode
	pub fn set_m(&mut self, m: NumOutMode) {
		unsafe { self.0.last_mut().unwrap_unchecked().3 = m; }
	}

	/// Current output precision
	pub fn get_k(&self) -> usize {
		unsafe { self.0.last().unwrap_unchecked().0 }
	}

	/// Current input base
	pub fn get_i(&self) -> &Natural {
		unsafe { &self.0.last().unwrap_unchecked().1 }
	}

	/// Current output base
	pub fn get_o(&self) -> &Natural {
		unsafe { &self.0.last().unwrap_unchecked().2 }
	}

	/// Current number output mode
	pub fn get_m(&self) -> NumOutMode {
		unsafe { self.0.last().unwrap_unchecked().3 }
	}
}
/// One entry of [`DEFAULT_PARAMS`].
impl Default for ParamStk {
	fn default() -> Self {
		let mut p = Self(Vec::new());
		p.create();
		p
	}
}
/// Length must be at least 1.
impl TryFrom<Vec<Params>> for ParamStk {
	type Error = ();

	fn try_from(value: Vec<Params>) -> Result<Self, Self::Error> {
		(!value.is_empty()).then_some(Self(value)).ok_or(())
	}
}

/// Combined interpreter state storage
///
/// Typically, just use the [`Default`]. Fields are public to allow for presets and extraction of values, see their documentation.
///
/// `regs` may contain thread handles, which are not copied with [`Clone`]. To preserve thread handles in `self`, use:
/// - `state.clear_vals();` instead of `state = State::default();`
/// - `state.replace_vals(other);` instead of `state = other;`
#[derive(Default, Debug, Clone)]
pub struct State {
	/// Main stack, [`Arc`] to allow shallow copies
	pub mstk: Vec<Arc<Value>>,

	/// Registers
	pub regs: RegStore,

	/// Parameters
	pub params: ParamStk
}
/// Export to state file with standard format
impl Display for State {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", String::from_utf8_lossy(&crate::STATE_FILE_HEADER))?;
		for (lri, lreg) in self.regs.low.iter().enumerate().filter(|(_, lreg)| !lreg.v.is_empty()) {
			for val in &lreg.v {
				writeln!(f, "{val}")?;
			}
			writeln!(f, "{lri}:ff")?;
		}
		for (hri, hreg) in self.regs.high.iter().filter(|(_, hreg)| !hreg.v.is_empty()) {
			for val in &hreg.v {
				writeln!(f, "{val}")?;
			}
			writeln!(f, "{}:ff", crate::num::nauto(hri, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2))?;
		}
		for val in &self.mstk {
			writeln!(f, "{val}")?;
		}
		write!(f, "{}", {
			let v: Vec<String> = self.params.inner().iter().map(|par| {
				let mut ps = String::new();
				if par.0 != DEFAULT_PARAMS.0 {ps += &format!("{{{}}}k", par.0);}
				if par.1 != DEFAULT_PARAMS.1 {ps += &format!("{{{}}}i", par.1);}
				if par.2 != DEFAULT_PARAMS.2 {ps += &format!("{{{}}}o", par.2);}
				if par.3 != DEFAULT_PARAMS.3 {ps += &format!("{{{}}}m", par.3 as u8);}
				ps
			}).collect();
			v.join("{")
		})?;
		Ok(())
	}
}
impl State {
	/// Reallocate all fields to fit, do not discard any stored data
	pub fn trim(&mut self) {
		self.mstk.shrink_to_fit();
		self.regs.trim();
		self.params.0.shrink_to_fit();
	}

	/// Clear all stored data, do not touch thread handles in `self`
	pub fn clear_vals(&mut self) {
		self.mstk = Vec::new();
		self.regs.clear_vals();
		self.params = ParamStk::default();
	}

	/// Replace stored data with contents of `other`, keep thread handles in `self` and discard any in `other`
	pub fn replace_vals(&mut self, other: Self) {
		self.mstk = other.mstk;
		self.regs.replace_vals(other.regs);
		self.params = other.params;
	}
}