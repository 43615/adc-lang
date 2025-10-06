use std::fmt::{Display, Formatter};
use malachite::Integer;
use crate::structs::Value;

#[derive(Debug)]
pub(crate) enum FnErr {
	Type1(TypeLabel),
	Type2(TypeLabel, TypeLabel),
	Type3(TypeLabel, TypeLabel, TypeLabel),
	Len2(usize, usize),
	Len3(usize, usize, usize),
	Index(Integer),
	Arith(String),
	Custom(String),
}
impl Display for FnErr {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Type1(ta) => {write!(f, "value of invalid type: {ta}")}
			Self::Type2(ta,tb) => {write!(f, "values of invalid types: {ta}, {tb}")}
			Self::Type3(ta,tb,tc) => {write!(f, "values of invalid types: {ta}, {tb}, {tc}")}
			Self::Len2(la, lb) => {write!(f, "arrays of different length: {la}, {lb}")}
			Self::Len3(la, lb, lc) => {write!(f, "arrays of different length: {la}, {lb}, {lc}")}
			Self::Index(ia) => {write!(f, "index out of range: {ia}")}
			Self::Arith(s) => {write!(f, "arithmetic error: {s}")}
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
		use Value::*;
		match value {
			B(_) => Self::B,
			N(_) => Self::N,
			S(_) => Self::S,
			A(_) => Self::A
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