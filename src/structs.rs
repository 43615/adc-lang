//! Storage structs and methods

#![allow(dead_code)] // TODO: make not dead

use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::sync::RwLock;
use std::thread::JoinHandle;
use malachite::{rational::Rational, Integer, Natural};
use regex::{Regex, RegexBuilder};
use bitvec::prelude::*;

#[derive(Debug)]
pub enum Value {
	B(BitVec),
	N(Rational),
	S(String),
	A(Vec<Value>)
}
impl Default for Value {
	#[inline(always)] fn default() -> Self {
		Self::A(Vec::new())
	}
}
/// Necessary custom implementation to avoid stack overflow due to deeply nested arrays
impl Drop for Value {
	fn drop(&mut self) {
		use Value::*;
		use std::mem::take;
		if let A(a) = self {
			let mut q: Vec<Vec<Value>> = vec![take(a)];	//init queue for arrays to be processed
			while let Some(mut a) = q.pop() {	//take current array
				for v in &mut a {	//traverse it
					if let A(aa) = v {	//if a nested array is encountered
						q.push(take(aa))	//move it to the queue
					}
				}
				//current array drops here, with no nested contents
			}
		}
		//scalar values drop here
	}
}

/// Necessary custom implementation to avoid stack overflow due to deeply nested arrays
impl Clone for Value {
	#[inline(always)] fn clone(&self) -> Self {
		use Value::*;
		match self {
			//traverse array using heap pseudorecursion, perform (cloning) identity function on every value
			//`exec1` takes over instead of continuing call recursion, `v.clone()` never gets called for arrays
			A(_) => crate::fns::exec1(|v, _| Ok(v.clone()), self, false).unwrap(),
			//scalar base cases
			B(b) => B(b.clone()),
			N(n) => N(n.clone()),
			S(s) => S(s.clone()),
		}
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
		self.high.retain(|_idx, reg| !reg.v.is_empty() || reg.th.is_some());
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
	#[inline(always)] pub(crate) fn get(&self, s: &String) -> Result<Regex, String> {
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

	#[inline(always)] pub(crate) fn clear(&self) {
		*self.0.write().unwrap() = HashMap::new();
	}
}

/// Hybrid iterator over UTF-8 strings, may be advanced by bytes or chars
///
/// Convertible [`From`] owned|borrowed × bytes|strings. Manually set nonzero or OOB positions are handled as expected.
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
	#[inline(always)] fn default() -> Self {
		Self::Owned { bytes: Vec::new(), pos: 0 }
	}
}

impl From<Vec<u8>> for Utf8Iter<'_> {
	#[inline(always)] fn from(bytes: Vec<u8>) -> Self {
		Self::Owned { bytes, pos: 0 }
	}
}
impl<'a> From<&'a [u8]> for Utf8Iter<'a> {
	#[inline(always)] fn from(bytes: &'a [u8]) -> Self {
		Self::Borrowed { bytes, pos: 0 }
	}
}
impl From<String> for Utf8Iter<'_> {
	#[inline(always)] fn from(s: String) -> Self {
		Self::Owned { bytes: s.into_bytes(), pos: 0 }
	}
}
impl<'a> From<&'a str> for Utf8Iter<'a> {
	#[inline(always)] fn from(s: &'a str) -> Self {
		Self::Borrowed { bytes: s.as_bytes(), pos: 0 }
	}
}

impl Iterator for Utf8Iter<'_> {
	type Item = u8;
	#[inline(always)] fn next(&mut self) -> Option<Self::Item> {
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
	/// Errors on invalid byte sequences and reports the erroneous values, `pos` does not change if an error occurs.
	pub fn try_next_char(&mut self) -> Result<char, String> {
		let (bytes, pos): (&[u8], &mut usize) = match self {
			Self::Borrowed {bytes, pos} => (bytes, pos),
			Self::Owned {bytes, pos} => (bytes, pos)
		};
		if let Some(b0) = bytes.get(*pos).cloned() {
			let c;
			match b0 {
				//within ASCII
				0x00..=0x7F => {
					c = b0 as u32;
					*pos += 1;
					Ok(unsafe{char::from_u32_unchecked(c)})
				}

				//continuation byte
				0x80..=0xBF => {
					Err(format!("Continuation byte at start of character: {b0:02X}"))
				}

				//2-byte char
				0xC2..=0xDF => {
					if let Some(b1) = bytes.get(*pos+1).cloned() {
						if !(0x80..=0xBF).contains(&b1) {
							return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X}"));
						}
						c = ((b0 & 0x1F) as u32) << 6
							| (b1 & 0x3F) as u32;
						*pos += 2;
						Ok(unsafe{char::from_u32_unchecked(c)})
					}
					else {
						Err(format!("Unexpected end of string: {b0:02X}"))
					}
				}

				//3-byte char
				0xE0..=0xEF => {
					if let Some(b1) = bytes.get(*pos+1).cloned() {
						if let Some(b2) = bytes.get(*pos+2).cloned() {
							if !(0x80..=0xBF).contains(&b1) {
								return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X} {b2:02X}"));
							}
							if !(0x80..=0xBF).contains(&b2) {
								return Err(format!("Non-continuation byte in character: {b0:02X} {b1:02X} {b2:02X}"));
							}
							c = ((b0 & 0x0F) as u32) << 12
								| ((b1 & 0x3F) as u32) << 6
								| (b2 & 0x3F) as u32;
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
					if let Some(b1) = bytes.get(*pos+1).cloned() {
						if let Some(b2) = bytes.get(*pos+2).cloned() {
							if let Some(b3) = bytes.get(*pos+3).cloned() {
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

				_ => Err(format!("Invalid byte in string: {b0:02X}")),
			}
		}
		else { 
			Err(format!("Position out of bounds: {}", *pos))
		}
	}
}

/// Number output mode
#[derive(Clone, Debug, Copy)]
#[repr(u8)] pub enum NumOutMode { Auto, Norm, Sci, Frac }

/// Stack for numeric IO parameter contexts (K,I,O), with checked accessors
#[derive(Clone, Debug)]
#[repr(transparent)] pub struct ParamStk(Vec<(Natural, Natural, Natural, NumOutMode)>);
impl ParamStk {
	/// Create new context with defaults 0,10,10
	#[inline(always)] pub fn create(&mut self) {
		self.0.push((0u8.into(), 10u8.into(), 10u8.into(), NumOutMode::Auto))
	}

	/// Return to previous context, create default if at bottom
	#[inline(always)] pub fn destroy(&mut self) {
		self.0.pop();
		if self.0.is_empty() {self.create();}
	}

	/// Discard all contexts, create default
	#[inline(always)] pub fn clear(&mut self) {
		*self = Self::default();
	}

	/// Checked edit of current output precision
	#[inline(always)] pub fn set_k(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() {
			self.0.last_mut().unwrap().0 = n;
			Ok(())
		}
		else {Err("Output precision must be a natural number")}
	}

	/// Checked edit of current input base
	#[inline(always)] pub fn set_i(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=2u8 {
			self.0.last_mut().unwrap().1 = n;
			Ok(())
		}
		else {Err("Input base must be a natural number >=2")}
	}

	/// Checked edit of current output base
	#[inline(always)] pub fn set_o(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=2u8 {
			self.0.last_mut().unwrap().2 = n;
			Ok(())
		}
		else {Err("Output base must be a natural number >=2")}
	}

	/// Set number output mode
	#[inline(always)] pub fn set_mode(&mut self, m: NumOutMode) {self.0.last_mut().unwrap().3 = m;}

	/// Current output precision
	#[inline(always)] pub fn k(&self) -> &Natural {&self.0.last().unwrap().0}

	/// Current input base
	#[inline(always)] pub fn i(&self) -> &Natural {&self.0.last().unwrap().1}

	/// Current output base
	#[inline(always)] pub fn o(&self) -> &Natural {&self.0.last().unwrap().2}

	/// Current number output mode
	#[inline(always)] pub fn mode(&self) -> NumOutMode {self.0.last().unwrap().3}
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