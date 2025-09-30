//! Impure commands that access the whole state, no rank-polymorphic application

#![allow(dead_code)]	//TODO: make not dead

use malachite::Rational;
use crate::structs::{State};

///Impure command
pub(crate) type Cmd = fn(&mut State) -> Result<(), &'static str>;
macro_rules! cmd {
    ($name:ident, $s: ident, $block:block) => {
		pub(crate) fn $name($s: &mut State) -> Result<(), &'static str> {
			$block
		}
	}
}

///Impure command with register access
pub(crate) type CmdR = fn(&mut State, &Rational) -> Result<(), &'static str>;
macro_rules! cmdr {
    ($name:ident, $s:ident, $ri:ident, $block:block) => {
		pub(crate) fn $name($s: &mut State, $ri: &Rational) -> Result<(), &'static str> {
			$block
		}
	}
}