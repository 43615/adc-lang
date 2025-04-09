//! Core parsing and primary API

pub mod structs;

pub mod fns;

pub mod cmds;

pub mod errors;

pub mod conv;

use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};
use lazy_static::lazy_static;
use phf_macros::phf_map;

lazy_static! {
	pub(crate) static ref RE_CACHE: structs::RegexCache = structs::RegexCache::default();
}

/// Debug flag (set by `-d` option in this crate's CLI binary wrapper). Information about executed commands will be printed to stderr.
pub static DEBUG: OnceLock<()> = OnceLock::new();

enum CmdType {
	///monadic pure function
	Fn1(fns::Mon),

	///dyadic pure function
	Fn2(fns::Dya),
	
	///triadic pure function
	Fn3(fns::Tri),

	///impure command
	Cmd(cmds::Cmd),

	///impure command with register access
	CmdR(cmds::CmdR),

	///`exec`-specific (macros, IO, OS...)
	Special,

	///begin value literal
	Lit,

	///no command
	Space,

	///invalid command
	Wrong,
}
impl Default for CmdType {
	#[inline(always)] fn default() -> Self {
		Self::Wrong
	}
}

static CMDS: phf::Map<char, CmdType> = {
	use CmdType::*;
	use fns::*;
	use cmds::*;
	phf_map! {
		'\0' => Space,
		'\t' => Space,
		'\n' => Space,
		'\r' => Space,
		' ' => Space,
		
		'\'' => Lit,
		'(' => Lit,
		'0' => Lit,
		'1' => Lit,
		'2' => Lit,
		'3' => Lit,
		'4' => Lit,
		'5' => Lit,
		'6' => Lit,
		'7' => Lit,
		'8' => Lit,
		'9' => Lit,
		'@' => Lit,
		'F' => Lit,
		'T' => Lit,
		'[' => Lit,
		
		'!' => Fn1(inv),
		'g' => Fn1(log),
		
		'%' => Fn2(r#mod),
		'*' => Fn2(mul),
		'+' => Fn2(add),
		'-' => Fn2(sub),
		'/' => Fn2(div),
		'G' => Fn2(logb),
		'^' => Fn2(pow),
		'~' => Fn2(euc),
		
		'|' => Fn3(bar),
		
		
	}
};