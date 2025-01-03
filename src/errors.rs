use std::error::Error;
use std::fmt::{Display, Formatter};
use malachite::Integer;
use crate::structs::Value;

#[derive(Debug)]
pub(crate) enum FnErr {
	Type1(TypeLabel),
	Type2(TypeLabel, TypeLabel),
	Type3(TypeLabel, TypeLabel, TypeLabel),
	Len(usize, usize),
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
			Self::Len(la,lb) => {write!(f, "arrays of different length: {la}, {lb}")}
			Self::Index(ia) => {write!(f, "index out of range: {ia}")}
			Self::Arith(s) => {write!(f, "arithmetic error: result is {s}")}
			Self::Custom(s) => {write!(f, "{s}")}
		}
	}
}
impl Error for FnErr {}

#[derive(Debug)]
pub(crate) enum TypeLabel {
	B, N, S, A
}
impl From<&Value> for TypeLabel {
	fn from(value: &Value) -> Self {
		use Value::*;
		match value {
			B(_) => Self::B,
			N(_) => Self::N,
			S(_) => Self::S,
			A(_)|AB(_) => Self::A
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