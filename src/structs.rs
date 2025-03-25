//! Storage structs and methods

use std::collections::{HashMap, VecDeque};
use std::sync::RwLock;
use std::thread::JoinHandle;
use std::mem::ManuallyDrop;
use malachite::{Rational, Integer, Natural};
use regex::{Regex, RegexBuilder};
use bit_vec::BitVec;

#[derive(Debug, Clone)]
pub enum Value {
	B(BitVec),
	N(Rational),
	S(String),
	A(Vec<Value>)
}

#[derive(Default)]
pub struct Register {
	v: Vec<Value>,
	th: Option<JoinHandle<Vec<Value>>>
}
static EMPTY_REG: Register = Register {
	v: Vec::new(),
	th: None
};

pub struct RegStore {
	low: [Register; 128],
	high: HashMap<Integer, Register>
}

impl std::ops::Index<&Integer> for RegStore {
	type Output = Register;

	fn index(&self, index: &Integer) -> &Self::Output {
		usize::try_from(index).ok().and_then(|i| self.low.get(i)).unwrap_or_else(|| {
			self.high.get(index).unwrap_or(&EMPTY_REG)
		})
	}
}
impl std::ops::IndexMut<&Integer> for RegStore {
	fn index_mut(&mut self, index: &Integer) -> &mut Self::Output {
		usize::try_from(index).ok().and_then(|i| self.low.get_mut(i)).unwrap_or_else(|| {
			if !self.high.contains_key(index) {
				self.high.insert(index.clone(), Register::default());
			}
			self.high.get_mut(index).unwrap()
		})
	}
}

#[derive(Default)]
#[repr(transparent)] pub struct RegexCache(pub RwLock<HashMap<String, Regex>>);

impl RegexCache {
	#[inline(always)] pub(crate) fn get(&self, s: &String) -> Result<Regex, String> {
		if let Some(re) = self.0.read().unwrap().get(s) {
			Ok(re.clone())
		}
		else {
			match RegexBuilder::new(s).build() {
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

/**
Parses the next `char` from a UTF-8 string starting at byte offset `i`, with no checks whatsoever.

Advances the offset in-place to the beginning of the next character.
# Safety
Assumes that `s` is valid UTF-8 and `i` is in bounds, ***undefined behavior*** otherwise.
*/
#[inline(always)] pub(crate) unsafe fn parse_utf8_unchecked(s: &str, i: &mut usize) -> char {
	let b = unchecked_index::unchecked_index(s.as_bytes());	//zero-cost conversions
	let c: u32;	//future char

	//within ASCII
	if b[*i] & 0x80 == 0 {
		c = b[*i] as u32;
		*i += 1;
	}

	//continuation bytes
	else if b[*i] & 0xC0 == 0 {std::hint::unreachable_unchecked()}

	//leading byte for 2-byte seq
	else if b[*i] & 0xE0 == 0 {
		c = ((b[*i] & 0x1F) as u32) << 6
			| (b[*i+1] & 0x3F) as u32;
		*i += 2;
	}

	//leading for 3
	else if b[*i] & 0xF0 == 0 {
		c = ((b[*i] & 0x0F) as u32) << 12
			| ((b[*i+1] & 0x3F) as u32) << 6
			| (b[*i+2] & 0x3F) as u32;
		*i += 3;
	}

	//leading for 4
	else {
		c = ((b[*i] & 0x07) as u32) << 18
			| ((b[*i+1] & 0x3F) as u32) << 12
			| ((b[*i+2] & 0x3F) as u32) << 6
			| (b[*i+3] & 0x3F) as u32;
		*i += 4;
	}

	char::from_u32_unchecked(c)
}

pub(crate) enum Macro {
	///reads straight from UTF-8, remembers position
	Once(
		String,
		usize
	),
	///preconverted to chars for O(1) indexing, remaining runs
	Multi(
		Vec<char>,
		usize,
		Natural
	)
}
impl Macro {
	pub(crate) fn new_once(s: String) -> Self {
		Self::Once(s, 0)
	}
	pub(crate) fn new_multi(s: String, n: Natural) -> Self {
		Self::Multi(
			{
				let mut v = Vec::new();
				let mut i = 0usize;

				while i<s.len() {
					v.push(unsafe{parse_utf8_unchecked(&s, &mut i)});
				}

				v
			},
			0, n-Natural::from(1u8))
	}

	///(Some(c), _): next char
	///(None, true): current run ended, start next one
	///(None, false): all runs used up, discard macro
	#[inline(always)] pub(crate) fn next(&mut self) -> (Option<char>, bool) {
		match self {
			Self::Once(s, i) => {
				if *i<s.len() { 
					(Some(unsafe{parse_utf8_unchecked(s, i)}), true)
				}
				else {(None, false)}
			},
			Self::Multi(s, i, n) => {
				if let Some(c) = s.get(*i) {*i+=1; (Some(*c), true)}
				else if *n == 0u8 {	//no runs left
					(None, false)
				}
				else {	//start next run
					*n -= Natural::from(1u8);
					*i = 0;
					(None, true)
				}
			}
		}
	}
}

/// Stack for number IO parameter contexts (K,I,O), with checked accessors
pub struct ParamStk(
	pub Vec<(Integer, Natural, Natural)>
);
impl ParamStk {
	/// Create new context with defaults 0,10,10
	#[inline(always)] pub fn create(&mut self) {
		self.0.push((0u8.into(), 10u8.into(), 10u8.into()))
	}
	
	/// Return to previous context, create default if at bottom
	#[inline(always)] pub fn destroy(&mut self) {
		self.0.pop();
		if self.0.is_empty() {self.create();}
	}

	/// Checked edit of current output precision
	#[inline(always)] pub fn set_k(&mut self, n: Integer) -> Result<(), &'static str> {
		if n>=0 {
			self.0.last_mut().unwrap().0 = n;
			Ok(())
		}
		else {Err("Output precision must be at least 0")}
	}

	/// Checked edit of current input base
	#[inline(always)] pub fn set_i(&mut self, n: Natural) -> Result<(), &'static str> {
		if n>=2 {
			self.0.last_mut().unwrap().1 = n;
			Ok(())
		}
		else {Err("Input base must be at least 2")}
	}

	/// Checked edit of current output base
	#[inline(always)] pub fn set_o(&mut self, n: Natural) -> Result<(), &'static str> {
		if n>=2 {
			self.0.last_mut().unwrap().2 = n;
			Ok(())
		}
		else {Err("Output base must be at least 2")}
	}

	/// Current output precision
	#[inline(always)] pub fn k(&self) -> &Integer {&self.0.last().unwrap().0}

	/// Current input base
	#[inline(always)] pub fn i(&self) -> &Natural {&self.0.last().unwrap().1}
	
	/// Current output base
	#[inline(always)] pub fn o(&self) -> &Natural {&self.0.last().unwrap().2}
}
impl Default for ParamStk {
	fn default() -> Self {
		let mut p = Self(Vec::new());
		p.create();
		p
	}
}

pub struct State {
	mstk: Vec<ManuallyDrop<Value>>,
	regs: RegStore,

}
