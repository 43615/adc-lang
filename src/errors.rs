//! The most boring module

use std::fmt::{Display, Formatter};
use crate::structs::Value;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FnErr {
	Type1(TypeLabel),
	Type2(TypeLabel, TypeLabel),
	Type3(TypeLabel, TypeLabel, TypeLabel),
	Index(usize),
	Len2(usize, usize),
	Len3(usize, usize, usize),
	Arith(String),
	Custom(String),
}
impl Display for FnErr {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Type1(ta) => {write!(f, "Wrong argument type: {ta}")}
			Self::Type2(ta,tb) => {write!(f, "Wrong argument types: {ta}, {tb}")}
			Self::Type3(ta,tb,tc) => {write!(f, "Wrong argument types: {ta}, {tb}, {tc}")}
			Self::Len2(la, lb) => {write!(f, "Arrays of different lengths: {la}, {lb}")}
			Self::Len3(la, lb, lc) => {write!(f, "Arrays of different lengths: {la}, {lb}, {lc}")}
			Self::Index(ia) => {write!(f, "Index out of range: {ia}")}
			Self::Arith(s) => {write!(f, "Arithmetic error: {s}")}
			Self::Custom(s) => {write!(f, "{s}")}
		}
	}
}
impl std::error::Error for FnErr {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)] pub enum TypeLabel {
	B, N, S, A
}
impl From<&Value> for TypeLabel {
	fn from(value: &Value) -> Self {
		match value {
			Value::B(_) => Self::B,
			Value::N(_) => Self::N,
			Value::S(_) => Self::S,
			Value::A(_) => Self::A
		}
	}
}

impl Display for TypeLabel {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}",
			match self {
				Self::B => {"boolean"}
				Self::N => {"number"}
				Self::S => {"string"}
				Self::A => {"array"}
			}
		)
	}
}

/// Custom rich error type for [`Utf8Iter`](crate::structs::Utf8Iter). Byte values are displayed using ADC's string literal syntax.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[must_use] pub enum Utf8Error {
	OutOfBounds(usize, usize),
	Impossible(u8),
	ContAtStart(u8),
	NonCont2(u8, u8),
	NonCont3(u8, u8, u8),
	NonCont4(u8, u8, u8, u8),
	Missing1(u8),
	Missing2(u8, u8),
	Missing3(u8, u8, u8),
	Overlong3(u32, u8, u8, u8),
	Overlong4(u32, u8, u8, u8, u8),
	Utf16Surr(u32, u8, u8, u8),
	TooLarge(u32, u8, u8, u8, u8)
}

impl Display for Utf8Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::OutOfBounds(pos, len) => {write!(f, "Position {pos} out of bounds, length is {len}")}
			Self::Impossible(b0) => {write!(f, "Impossible byte in string: [\\{b0:02X}]")}
			Self::ContAtStart(b0) => {write!(f, "Continuation byte at start of character: [\\{b0:02X}]")}
			Self::NonCont2(b0, b1) => {write!(f, "Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}]")}
			Self::NonCont3(b0, b1, b2) => {write!(f, "Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]")}
			Self::NonCont4(b0, b1, b2, b3) => {write!(f, "Non-continuation byte in character: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]")}
			Self::Missing1(b0) => {write!(f, "Unexpected end of string: [\\{b0:02X}]")}
			Self::Missing2(b0, b1) => {write!(f, "Unexpected end of string: [\\{b0:02X}\\{b1:02X}]")}
			Self::Missing3(b0, b1, b2) => {write!(f, "Unexpected end of string: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]")}
			Self::Overlong3(c, b0, b1, b2) => {write!(f, "Overlong encoding of U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]")}
			Self::Overlong4(c, b0, b1, b2, b3) => {write!(f, "Overlong encoding of U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]")}
			Self::Utf16Surr(c, b0, b1, b2) => {write!(f, "Unexpected UTF-16 surrogate U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}]")}
			Self::TooLarge(c, b0, b1, b2, b3) => {write!(f, "Out-of-range character U+{c:04X}: [\\{b0:02X}\\{b1:02X}\\{b2:02X}\\{b3:02X}]")}
		}
	}
}
impl std::error::Error for Utf8Error {}