//! Impure commands that access the whole state, no rank-polymorphic application

#![allow(dead_code)]	//TODO: make not dead

use malachite::Integer;
use crate::structs::{State};

///Impure command
pub(crate) type Cmd = fn(&mut State) -> Result<(), &'static str>;
macro_rules! cmd {
    ($name:ident, $s: ident, $block:stmt) => {
		#[inline(always)] pub(crate) fn $name($s: &mut State) -> Result<(), &'static str> {
			$block
		}
	}
}

///Impure command with register access
pub(crate) type CmdR = fn(&mut State, &Integer) -> Result<(), &'static str>;
macro_rules! cmdr {
    ($name:ident, $s:ident, $ri:ident, $block:stmt) => {
		#[inline(always)] pub(crate) fn $name($s: &mut State, $ri: &Integer) -> Result<(), &'static str> {
			$block
		}
	}
}

///TODO: remove
cmd!(phc,_s, Ok(()));

///TODO: remove
cmdr!(phr,_s,_ri, Ok(()));