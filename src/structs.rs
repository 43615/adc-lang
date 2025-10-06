//! Storage structs and methods

#![allow(dead_code)] // TODO: make not dead

use bitvec::prelude::*;
use malachite::{Natural, Rational};
use regex::{Regex, RegexBuilder};
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::sync::RwLock;
use std::thread::JoinHandle;

#[derive(Debug)]
pub enum Value {
	B(BitVec),
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
			A(_) => crate::fns::exec1(|v, _| Ok(v.clone()), self, false).unwrap()
		}
	}
}

impl Value {
	/// Prints scalar values, passing `A` is undefined behavior.
	fn display_scalar(&self, k: usize, o: &Natural, nm: NumOutMode) -> String {
		use Value::*;
		match self {
			B(b) => {
				b.iter().by_vals().rev().map(|b| if b {'T'} else {'F'}).collect()
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
				String::from("[") + s + "]"
			},
			A(_) => {
				unsafe { std::hint::unreachable_unchecked() }
			}
		}
	}
	
	/// Prints the value, heap DFS to avoid overflowing the stack.
	pub fn display(&self, k: usize, o: &Natural, nm: NumOutMode) -> String {
		use Value::*;
		match self {
			A(a) => {	//the fun part
				let mut stk: Vec<std::slice::Iter<Value>> = vec![a.iter()];
				let mut res = String::from('(');
				while let Some(i) = stk.last_mut() {	//keep operating on the top iter
					if let Some(val) = i.next() {	//advance it
						match val {
							A(aa) => {	//nested array encountered
								res.push('(');
								stk.push(aa.iter());	//"recursive call"
							},
							_ => {	//scalar encountered
								res += &val.display_scalar(k, o, nm);	//append it
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
				self.display_scalar(k, o, nm)
			}
		}
	}
}

/// Default printing function, `N` uses output base 10 and automatic format.
impl Display for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.display(0, &Natural::const_from(10), NumOutMode::Auto))
	}
}

#[derive(Default, Debug)]
pub struct Register {
	pub v: Vec<Rc<Value>>,
	pub th: Option<JoinHandle<Vec<Value>>>
}
/// removes thread handles since they are unique
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
	/// get register reference, don't create a register if not present
	pub(crate) fn try_get(&self, index: &Rational) -> Option<&Register> {
		usize::try_from(index).ok()
			.and_then(|i| self.low.get(i))
			.or_else(|| self.high.get(index))
	}

	/// get mutable register reference, create register if necessary
	pub(crate) fn get_mut(&mut self, index: &Rational) -> &mut Register {
		usize::try_from(index).ok()
			.and_then(|i| self.low.get_mut(i))
			.unwrap_or_else(|| {
				if !self.high.contains_key(index) {
					self.high.insert(index.clone(), Register::default());
				}
				self.high.get_mut(index).unwrap()
			})
	}

	/// discard empty registers and reallocate
	pub(crate) fn trim(&mut self) {
		self.high.retain(|_, reg| !reg.v.is_empty() || reg.th.is_some());
		self.high.shrink_to_fit();
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

/// Hybrid iterator over UTF-8 strings, may be advanced by bytes or chars.
///
/// Convertible [`From`] owned|borrowed × bytes|strings. Manually set nonzero or OOB positions are handled as expected.
///
/// [`Self::try_next_char`] handles all possible invalid UTF-8 sequences, and is used for [`TryInto<String>`].
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
			res.push(value.try_next_char()?);
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
	/// Errors on invalid byte sequences and reports the erroneous values, `pos` only advances if parsing is successful.
	///
	/// Handles all possible UTF-8 encoding errors:
	/// - Invalid bytes: C0, C1, F5–FF
	/// - Character starting with a continuation byte (80–BF)
	/// - Multi-byte character with missing bytes
	/// - Non-continuation byte in multi-byte character
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
					Err(format!("Continuation byte at start of character: {b0:02X}"))
				}

				//2-byte char
				0xC2..=0xDF => {
					if let Some(b1) = bytes.get(*pos+1).copied() {
						if !(0x80..=0xBF).contains(&b1) {
							return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X}"));
						}
						c = ((b0 & 0x1F) as u32) << 6
							| (b1 & 0x3F) as u32;
						//all good:
						*pos += 2;
						Ok(unsafe{char::from_u32_unchecked(c)})
					}
					else {
						Err(format!("Unexpected end of string: {b0:02X}"))
					}
				}

				//3-byte char
				0xE0..=0xEF => {
					if let Some(b1) = bytes.get(*pos+1).copied() {
						if let Some(b2) = bytes.get(*pos+2).copied() {
							if !(0x80..=0xBF).contains(&b1) {
								return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X} {b2:02X}"));
							}
							if !(0x80..=0xBF).contains(&b2) {
								return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X} {b2:02X}"));
							}
							c = ((b0 & 0x0F) as u32) << 12
								| ((b1 & 0x3F) as u32) << 6
								| (b2 & 0x3F) as u32;
							if c < 0x0800 {
								return Err(format!("Overlong encoding of U+{c:04X}: {b0:02X} {b1:02X} {b2:02X}"));
							}
							if (0xD800u32..=0xDFFFu32).contains(&c) {
								return Err(format!("Unexpected UTF-16 surrogate U+{c:04X}: {b0:02X} {b1:02X} {b2:02X}"));
							}
							//all good:
							*pos += 3;
							Ok(unsafe{char::from_u32_unchecked(c)})
						}
						else {
							Err(format!("Unexpected end of string: {b0:02X} {b1:02X}"))
						}
					}
					else {
						Err(format!("Unexpected end of string: {b0:02X}"))
					}
				}

				//4-byte char
				0xF0..=0xF4 => {
					if let Some(b1) = bytes.get(*pos+1).copied() {
						if let Some(b2) = bytes.get(*pos+2).copied() {
							if let Some(b3) = bytes.get(*pos+3).copied() {
								if !(0x80..=0xBF).contains(&b1) {
									return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X} {b2:02X} {b3:02X}"));
								}
								if !(0x80..=0xBF).contains(&b2) {
									return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X} {b2:02X} {b3:02X}"));
								}
								if !(0x80..=0xBF).contains(&b3) {
									return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X} {b2:02X} {b3:02X}"));
								}
								c = ((b0 & 0x07) as u32) << 18
									| ((b1 & 0x3F) as u32) << 12
									| ((b2 & 0x3F) as u32) << 6
									| (b3 & 0x3F) as u32;
								if c < 0x10000 {
									return Err(format!("Overlong encoding of U+{c:04X}: {b0:02X} {b1:02X} {b2:02X} {b3:02X}"));
								}
								if c > 0x10FFFF {
									return Err(format!("Out-of-range character U+{c:04X}: {b0:02X} {b1:02X} {b2:02X} {b3:02X}"));
								}
								//all good:
								*pos += 4;
								Ok(unsafe{char::from_u32_unchecked(c)})
							}
							else {
								Err(format!("Unexpected end of string: {b0:02X} {b1:02X} {b2:02X}"))
							}
						}
						else {
							Err(format!("Unexpected end of string: {b0:02X} {b1:02X}"))
						}
					}
					else {
						Err(format!("Unexpected end of string: {b0:02X}"))
					}
				}

				0xC0 | 0xC1 | 0xF5.. => Err(format!("Invalid byte in string: {b0:02X}")),
			}
		}
		else { 
			Err(format!("Position out of bounds: {}", *pos))
		}
	}
	
	pub fn is_finished(&self) -> bool {
		let (bytes, pos): (&[u8], &usize) = match self {
			Self::Borrowed {bytes, pos} => (bytes, pos),
			Self::Owned {bytes, pos} => (bytes, pos)
		};
		bytes.len() <= *pos
	}
}

/// Number output mode
#[derive(Clone, Debug, Copy)]
#[repr(u8)] pub enum NumOutMode { Auto, Norm, Sci, Frac }

/// Stack for numeric IO parameter contexts (K,I,O), with checked accessors
#[derive(Clone, Debug)]
#[repr(transparent)] pub struct ParamStk(Vec<(usize, Natural, Natural, NumOutMode)>);
impl ParamStk {
	/// Create new context with defaults 0,10,10,auto
	pub fn create(&mut self) {
		self.0.push((0, Natural::const_from(10), Natural::const_from(10), NumOutMode::Auto))
	}

	/// Return to previous context, create default if at bottom
	pub fn destroy(&mut self) {
		self.0.pop();
		if self.0.is_empty() {self.create();}
	}

	/// Discard all contexts, create default
	pub fn clear(&mut self) {
		*self = Self::default();
	}

	/// Checked edit of current output precision
	pub fn set_k(&mut self, r: &Rational) -> Result<(), String> {
		if let Ok(u) = r.try_into() {
			self.0.last_mut().unwrap().0 = u;
			Ok(())
		}
		else {Err(format!("Output precision must be a natural number <={}", usize::MAX))}
	}

	/// Checked edit of current input base
	pub fn set_i(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=2u8 {
			self.0.last_mut().unwrap().1 = n;
			Ok(())
		}
		else {Err("Input base must be a natural number >=2")}
	}

	/// Checked edit of current output base
	pub fn set_o(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=2u8 {
			self.0.last_mut().unwrap().2 = n;
			Ok(())
		}
		else {Err("Output base must be a natural number >=2")}
	}

	/// Set number output mode
	pub fn set_mode(&mut self, m: NumOutMode) {self.0.last_mut().unwrap().3 = m;}

	/// Current output precision
	pub fn k(&self) -> usize {self.0.last().unwrap().0}

	/// Current input base
	pub fn i(&self) -> &Natural {&self.0.last().unwrap().1}

	/// Current output base
	pub fn o(&self) -> &Natural {&self.0.last().unwrap().2}

	/// Current number output mode
	pub fn mode(&self) -> NumOutMode {self.0.last().unwrap().3}
}
impl Default for ParamStk {
	fn default() -> Self {
		let mut p = Self(Vec::new());
		p.create();
		p
	}
}

/// Combined interpreter state storage
///
/// Typically, just use the [`Default`]. Fields are public to allow for presets and extraction of values, check their documentation.
#[derive(Default, Clone, Debug)]
pub struct State {
	/// main stack, [`Rc`] to allow shallow copies
	pub mstk: Vec<Rc<Value>>,

	/// registers
	pub regs: RegStore,

	/// parameters
	pub params: ParamStk,

	/// register pointer
	pub rptr: Option<Rational>,

	/// array pointer
	pub aptr: Vec<usize>
}
/// Export to state file with standard format
impl Display for State {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		todo!()
	}
}