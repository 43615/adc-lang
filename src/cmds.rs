//! Impure commands that access the whole state, manual application.
//!
//! New resulting values are written to a `Vec` to enable redirection to array input.

use crate::structs::{State, Value};
use std::sync::Arc;
use malachite::Rational;

/// Impure command
pub(crate) type Cmd = fn(&mut State) -> Result<Vec<Value>, String>;
/// Function template for impure command
#[macro_export] macro_rules! cmd {
    ($name:ident, $st:ident $block:block) => {
		pub(crate) fn $name($st: &mut State) -> Result<Vec<Value>, String> {
			$block
		}
	}
}

/// Impure command with register access
pub(crate) type CmdR = fn(&mut State, &Rational) -> Result<Vec<Value>, String>;
/// Function template for impure command with register access
#[macro_export] macro_rules! cmdr {
    ($name:ident, $st:ident, $ri:ident $block:block) => {
		pub(crate) fn $name($st: &mut State, $ri: &Rational) -> Result<Vec<Value>, String> {
			$block
		}
	}
}