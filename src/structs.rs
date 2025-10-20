//! Storage structs and methods

#![allow(dead_code)] // TODO: make not dead

use bitvec::prelude::*;
use malachite::{Natural, Rational};
use regex::{Regex, RegexBuilder};
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use std::sync::mpsc::Sender;
use std::sync::{Arc, RwLock};
use std::thread as th;
use const_format::concatcp;
use crate::STATE_FILE_HEADER;

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
			A(_) => unsafe { crate::fns::exec1(|v, _| Ok(v.clone()), self, false).unwrap_unchecked() }
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

/// Default printing function, `N` uses default parameters.
impl Display for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.display(DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2, DEFAULT_PARAMS.3))
	}
}

#[derive(Default, Debug)]
pub struct Register {
	/// Stack of values, [`Arc`] to allow shallow copies
	pub v: Vec<Arc<Value>>,
	/// Thread handle with result values, kill signal
	pub th: Option<(th::JoinHandle<Vec<Value>>, Sender<()>)>
}
/// Remove thread handles since they are unique
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
	pub fn try_get(&self, index: &Rational) -> Option<&Register> {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get(u))
			.or_else(|| self.high.get(index))
	}

	/// get mutable register reference, create register if necessary
	pub fn get_mut(&mut self, index: &Rational) -> &mut Register {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get_mut(u))
			.unwrap_or_else(|| self.high.entry(index.clone()).or_default())
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

				0xC0 | 0xC1 | 0xF5.. => Err(format!("Invalid byte in string: [\\{b0:02X}]")),
			}
		}
		else { 
			Err(format!("Position out of bounds: {}", *pos))
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
#[derive(Clone, Debug, Copy)]
#[repr(u8)] pub enum NumOutMode { Auto=0, Norm=1, Sci=2, Frac=3 }

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
	/// Creates new context with default values
	pub fn create(&mut self) {
		self.0.push(DEFAULT_PARAMS)
	}

	/// Returns to previous context, create default if at bottom
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

	/// Creates [`Self`] from an underlying [`Vec`], length must be at least 1
	pub fn try_from_inner(v: Vec<Params>) -> Option<Self> {
		(!v.is_empty()).then_some(Self(v))
	}

	/// Checked edit of current output precision
	pub fn try_set_k(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(u) = r.try_into() {
			unsafe { self.0.last_mut().unwrap_unchecked().0 = u; }
			Ok(())
		}
		else {Err(concatcp!("Output precision must be a natural number <={}", usize::MAX))}
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

	/// Set number output mode
	pub fn try_set_m(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(u) = u8::try_from(r) && u<=3 {
			unsafe { self.0.last_mut().unwrap_unchecked().3 = std::mem::transmute(u); }
			Ok(())
		}
		else {Err("Output mode must be 0|1|2|3")}
		
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
impl Default for ParamStk {
	fn default() -> Self {
		let mut p = Self(Vec::new());
		p.create();
		p
	}
}

/// Combined interpreter state storage
///
/// Typically, just use the [`Default`]. Fields are public to allow for presets and extraction of values, see their documentation.
#[derive(Default, Clone, Debug)]
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
		write!(f, "{}", String::from_utf8_lossy(&STATE_FILE_HEADER))?;
		for (lri, lreg) in self.regs.low.iter().enumerate() {
			for val in &lreg.v {
				writeln!(f, "{val}")?;
			}
			writeln!(f, "{lri}:ff")?;
		}
		for (hri, hreg) in &self.regs.high {
			for val in &hreg.v {
				writeln!(f, "{val}")?;
			}
			writeln!(f, "{}:ff", crate::num::nauto(hri, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2))?;
		}
		for val in &self.mstk {
			writeln!(f, "{val}")?;
		}
		write!(f, "{}", {
			let v: Vec<String> = self.params.inner().iter().map(|par| format!("{}k{}i{}o{}m", par.0, par.1, par.2, par.3 as u8)).collect();
			v.join("{")
		})?;
		Ok(())
	}
}
impl State {
	/// Reallocate all fields to fit, do not discard any stored data
	pub fn trim(&mut self) {
		self.mstk.shrink_to_fit();
		self.regs.high.retain(|_, reg| !reg.v.is_empty() || reg.th.is_some());
		self.regs.high.shrink_to_fit();
		self.params.0.shrink_to_fit();
	}
}