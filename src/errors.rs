use std::fmt::{Display, Formatter};
use crate::structs::Value;

#[derive(Debug)]
pub(crate) enum FnErr {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)] pub(crate) enum TypeLabel {
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