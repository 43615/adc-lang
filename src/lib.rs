//! Core parsing and primary API

pub mod structs;
use structs::*;

pub(crate) mod fns;
use fns::*;

pub(crate) mod cmds;
use cmds::*;

pub(crate) mod errors;
use errors::*;

pub(crate) mod conv;
use conv::*;

pub(crate) mod num;
use num::*;

use std::collections::HashMap;
use std::io::{BufRead, Write};
use lazy_static::lazy_static;
use malachite::Rational;

/// Added at the start of saved state files
pub const STATE_FILE_HEADER: [u8;20] = *b"# ADC state file v1\n";

/// Specifies the line editor to use, with a default config
type InnerEditor = rustyline::Editor<(), rustyline::history::MemHistory>;
#[repr(transparent)] struct LineEditor(InnerEditor);
impl Default for LineEditor {
	fn default() -> Self {
		let conf = rustyline::Config::builder()
			.auto_add_history(true)
			.max_history_size(usize::MAX).unwrap()
			.build();

		Self(
			InnerEditor::with_history(
			conf.clone(),
			rustyline::history::MemHistory::with_config(&conf)
			).unwrap()
		)
	}
}

/// Generic input stream adapter trait, used for adding a proper line editor.
///
/// Has a generic implementation for all [`BufRead`] types, `prompt` does nothing in that case.
pub trait ReadLine {
	/// Display `prompt` if possible and read one line of input. The definition of "line" is not strict.
	/// 
	/// This is typically called once by every execution of the `?` command within ADC.
	fn read_line(&mut self, prompt: &str) -> std::io::Result<String>;
}
impl<T: BufRead> ReadLine for T {
	fn read_line(&mut self, _prompt: &str) -> std::io::Result<String> {
		let mut buf = String::new();
		self.read_line(&mut buf)?;
		Ok(buf)
	}
}
impl ReadLine for LineEditor {
	fn read_line(&mut self, prompt: &str) -> std::io::Result<String> {
		self.0.readline(prompt).map_err(|re| {
			use rustyline::error::ReadlineError::*;
			use std::io::Error;
			match re {
				Io(e) => e,
				Eof => Error::other("EOF"),
				Interrupted => Error::other("Interrupted"),
				Errno(e) => Error::other(e),
				Signal(_) => Error::other("Interrupted"),
				_ => Error::other("Unknown input error")
			}
		})
	}
}

/// Bundle of standard IO streams, generic interface to support custom IO wrappers
pub struct IOStreams (
	/// Input
	pub Box<dyn ReadLine>,
	/// Output
	pub Box<dyn Write>,
	/// Error
	pub Box<dyn Write>
);
impl IOStreams {
	/// Use dummy IO streams, these do nothing
	pub fn empty() -> Self {
		Self (
			Box::new(std::io::empty()),
			Box::new(std::io::empty()),
			Box::new(std::io::empty())
		)
	}

	/// Use IO streams of the process (stdin, stdout, stderr)
	pub fn process() -> Self {
		Self (
			Box::new(LineEditor::default()),
			Box::new(std::io::stdout()),
			Box::new(std::io::stderr())
		)
	}
}

/// How much information to output on stderr
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LogLevel {
	/// Only output error messages
	Normal,
	/// Report every command (without values) 
	Debug,
	/// No error messages, stderr disabled
	Quiet
}

lazy_static! {
	pub(crate) static ref RE_CACHE: RegexCache = RegexCache::default();
}

enum Command {
	///monadic pure function
	Fn1(Mon),

	///dyadic pure function
	Fn2(Dya),
	
	///triadic pure function
	Fn3(Tri),

	///impure command
	Cmd(Cmd),

	///impure command with register access
	CmdR(CmdR),

	///`exec`-specific (macros, IO, OS...)
	Special,

	///begin value literal
	Lit,

	///no command
	Space,

	///invalid command
	Wrong,
}
impl Default for Command {
	fn default() -> Self {
		Self::Wrong
	}
}

static CMDS: phf::Map<u8, Command> = {
	use Command::*;
	use fns::*;
	use cmds::*;
	phf::phf_map! {
		b'\0' | b'\t' | b'\n' | b'\r' | b' ' => Space,
		
		b'\'' | b'(' | b')' | b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' | b'@' | b'F' | b'T' | b'[' => Lit,
		
		b'!' => Fn1(neg),
		b'g' => Fn1(log),
		
		b'%' => Fn2(r#mod),
		b'*' => Fn2(mul),
		b'+' => Fn2(add),
		b'-' => Fn2(sub),
		b'/' => Fn2(div),
		b'G' => Fn2(logb),
		b'^' => Fn2(pow),
		b'~' => Fn2(euc),
		
		b'|' => Fn3(bar),
		
		
	}
};

/// Results of running [`exec`], wrappers should handle these differently
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExecResult {
	/// Commands ran to completion, request further input
	Finished,

	/// Exit of current instance requested (q), end context
	SoftQuit(u8),

	/// Complete exit requested (`q), terminate
	HardQuit(u8)
}

/// Interpreter entry point, executes ADC commands to modify state
///
/// # Arguments
/// - `st`: State struct to work on, modified in-place
/// - `start`: Initial commands to run
/// - `io`: Bundle of IO stream handles
///   - `io.0`: Input, read by ? one line at a time
///   - `io.1`: Output, written to by printing commands
///   - `io.2`: Error messages, one per line
/// - `ll`: Level of verbosity for `io.2`
/// - `strict`: Restricted mode switch, prevents OS access (for untrusted input). If `false`, the interpreter may read/write files and execute OS commands, subject to any OS-level permissions.
#[cold] #[inline(never)] pub fn exec(st: &mut State, start: Utf8Iter, io: &mut IOStreams, mut ll: LogLevel, mut strict: bool) -> ExecResult {
	use ExecResult::*;
	use malachite::{Rational, Natural};
	let rats = 
	[
		Rational::const_from_signeds(1,8),
	];
	let k = 0;
	let o = Natural::const_from(10);
	
	for rat in rats {
		let (neg, iparts, fparts, rparts) = digits(&rat, k, &o);
		println!("{}", nnorm(neg, &iparts, &fparts, &rparts, &o));
		println!("{}", nsci(neg, &iparts, &fparts, &rparts, &o));
		println!("{}", nfrac(&rat, 0, &o));
		println!("{}\n\n", nauto(&rat, k, &o));
	}
	
	Finished
}