//! ADC interpreter API. Many safe items are public for manual access.

pub mod structs;
use structs::*;

pub(crate) mod fns;

pub(crate) mod cmds;

pub(crate) mod errors;

pub(crate) mod conv;

pub(crate) mod num;

#[cfg(not(feature = "no_os"))]
mod os;

use lazy_static::lazy_static;
use std::cell::LazyCell;
use std::io::{BufRead, ErrorKind, Write};
use std::ops::Bound;
use std::ptr::NonNull;
use std::sync::mpsc::{Receiver, TryRecvError};
use std::sync::{Arc, Mutex};
use std::thread as th;
use malachite::{Integer, Natural, Rational};
use malachite::base::num::random::RandomPrimitiveInts;
use malachite::natural::random::get_random_natural_less_than;
use malachite::base::num::basic::traits::{NegativeOne, One, Zero};
use malachite::base::num::conversion::traits::{ConvertibleFrom, PowerOf2DigitIterable, RoundingFrom, WrappingFrom};
use malachite::base::rounding_modes::RoundingMode;

/// Added at the start of saved state files
pub const STATE_FILE_HEADER: [u8;20] = *b"# ADC state file v1\n";

/// Specifies the line editor to use, with a default config
type InnerEditor = rustyline::Editor<(), rustyline::history::MemHistory>;
#[repr(transparent)] struct LineEditor(InnerEditor);
impl Default for LineEditor {
	fn default() -> Self {
		let conf = rustyline::Config::builder()
			.auto_add_history(true)
			.enable_signals(true)
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
/// Comes with a generic implementation for all [`BufRead`] types, `prompt` does nothing in that case.
pub trait ReadLine {
	/// Display `prompt` if possible and read one line of input. The definition of "line" is not strict.
	/// 
	/// This is typically called once by every execution of the `?` command within ADC.
	///
	/// [`ErrorKind::Interrupted`] causes `?` to error, [`ErrorKind::UnexpectedEof`] makes an empty string, other [`ErrorKind`]s are returned early from the interpreter.
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
			use rustyline::error::{ReadlineError::*, Signal};
			use std::io::Error;
			match re {
				Io(e) => e,
				Eof => Error::from(ErrorKind::UnexpectedEof),
				Interrupted | Signal(Signal::Interrupt) => Error::from(ErrorKind::Interrupted),
				Errno(e) => Error::from(e),
				Signal(Signal::Resize) => Error::other("Window size changed"),
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

	/// Use IO streams of the process (stdin, stdout, stderr), with extra line editor on stdin
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

fn rng_preset(bytes: [u8; 32]) -> RandomPrimitiveInts<u64> {
	malachite::base::num::random::random_primitive_ints(malachite::base::random::Seed::from_bytes(bytes))
}

fn rng_os() -> RandomPrimitiveInts<u64> {
	let mut bytes = [0u8; 32];
	getrandom::fill(&mut bytes).unwrap();
	rng_preset(bytes)
}

#[derive(Default, Clone, Copy)]
enum Command {
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
	#[default] Wrong,
}

/// Dense map of bytes to commands. SAFETY: Length must be exactly 256.
const CMDS: [Command; 256] = {
	use Command::*;
	use fns::*;
	use cmds::*;
	[
		//NUL		SOH			STX			ETX			EOT			ENQ			ACK			BEL			BS			HT			LF			VT			FF			CR			SO			SI
		Space,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Space,		Space,		Space,		Space,		Space,		Wrong,		Wrong,

		//DLE		DC1			DC2			DC3			DC4			NAK			SYN			ETB			CAN			EM			SUB			ESC			FS			GS			RS			US
		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,

		//SP		!			"			#			$			%			&			'			(			)			*			+			,			-			.			/
		Space,		Fn1(neg),	Wrong,		Space,		Wrong,		Fn2(r#mod),	Wrong,		Lit,		Special,	Special,	Fn2(mul),	Fn2(add),	Wrong,		Fn2(sub),	Lit,		Fn2(div),

		//0			1			2			3			4			5			6			7			8			9			:			;			<			=			>			?
		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Special,	Wrong,		Wrong,		Wrong,		Wrong,		Special,

		//@			A			B			C			D			E			F			G			H			I			J			K			L			M			N			O
		Lit,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Lit,		Fn2(logb),	Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,

		//P			Q			R			S			T			U			V			W			X			Y			Z			[			\			]			^			_
		Special,	Special,	Wrong,		Wrong,		Lit,		Wrong,		Wrong,		Wrong,		Special,	Wrong,		Wrong,		Lit,		Special,	Wrong,		Fn2(pow),	Special,

		//`			a			b			c			d			e			f			g			h			i			j			k			l			m			n			o
		Special,	Special,	Wrong,		Wrong,		Wrong,		Wrong,		Special,	Fn1(log),	Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,

		//p			q			r			s			t			u			v			w			x			y			z			{			|			}			~			DEL
		Special,	Special,	Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Special,	Wrong,		Fn1(disc),	Wrong,		Fn3(bar),	Wrong,		Fn2(euc),	Wrong,

		//~~description of what i'm doing:~~ non-ASCII:
		Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,
		Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,
		Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,
		Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong,Wrong
	]
};

fn byte_cmd(b: u8) -> Command {
	unsafe {	//SAFETY: length is 256
		*CMDS.get_unchecked(b as usize)
	}
}

fn string_or_bytes(v: &[u8]) -> String {
	str::from_utf8(v).map(|s| s.to_owned()).unwrap_or_else(|_| {
		let mut res = String::from("(not UTF-8: [");
		for b in v {
			res += &format!("\\{b:02X}");
		}
		res += "])";
		res
	})
}

/// Results of running [`interpreter`], wrappers should handle these differently
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExecResult {
	/// Commands ran to completion, request further input
	Finished,

	/// Exit of current instance requested (q), end context
	SoftQuit(std::process::ExitCode),

	/// Complete exit requested (`q), terminate
	HardQuit(std::process::ExitCode)
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
/// - `kill`: Receiver for a kill signal from a parent thread, checked with `try_recv` in the command parsing loop
/// - `restrict`: Restricted mode switch, prevents OS access (for untrusted input). If `false`, the interpreter may read/write files and execute OS commands, subject to any OS-level permissions.
///
/// # Errors
/// Any IO errors that arise when accessing the IO streams are returned early, aborting the interpreter. Keep that possibility to a minimum when preparing custom IO streams.
///
/// # Panics
/// Shouldn't™
#[cold] #[inline(never)] pub fn interpreter(
		st: &mut State,
		start: Utf8Iter,
		io: &Mutex<IOStreams>,
		mut ll: LogLevel,
		kill: Option<&Receiver<()>>,
		mut restrict: bool
	) -> std::io::Result<ExecResult>
{
	use ExecResult::*;

	let th_name = th::current().name().unwrap_or_default().to_owned();

	let mut pbuf: Option<String> = None;	//print-to-string buffer

	let mut elatch: Option<(Natural, char, String)> = None;

	macro_rules! out {
    	($s:expr) => {
			if let Some(s) = &mut pbuf {
				s.push_str($s);
			}
			else {
				let out = &mut io.lock().unwrap().1;
				write!(out, "{}", $s)?;
				out.flush()?;
			}
		};
		($f:literal, $($s:expr),*) => {
			if let Some(s) = &mut pbuf {
				s.push_str(&format!($f, $($s),*));
			}
			else {
				let out = &mut io.lock().unwrap().1;
				write!(out, "{}", format!($f, $($s),*))?;
				out.flush()?;
			}
		};
	}

	macro_rules! outln {
    	($s:expr) => {
			if let Some(s) = &mut pbuf {
				s.push_str($s);
				s.push('\n');
			}
			else {
				let out = &mut io.lock().unwrap().1;
				writeln!(out, "{}", $s)?;
				out.flush()?;
			}
		};
		($f:literal, $($s:expr),*) => {
			if let Some(s) = &mut pbuf {
				s.push_str(&format!($f, $($s),*));
				s.push('\n');
			}
			else {
				let out = &mut io.lock().unwrap().1;
				writeln!(out, "{}", format!($f, $($s),*))?;
				out.flush()?;
			}
		};
	}

	macro_rules! synerr {
		($c:expr, $s:expr) => {
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "! {th_name}{}", $s)?;
				err.flush()?;
			}
			elatch = Some((Natural::ZERO, $c, $s.into()));
		};
    	($c:expr, $f:literal, $($s:expr),*) => {
			let s = format!($f, $($s),*);
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "! {th_name}{}", s)?;
				err.flush()?;
			}
			elatch = Some((Natural::ZERO, $c, s));
		};
	}

	macro_rules! valerr {
    	($c:expr, $s:expr) => {
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "? {th_name}: {}", $s)?;
				err.flush()?;
			}
			elatch = Some((Natural::ZERO, $c, $s.into()));
		};
		($c:expr, $f:literal, $($s:expr),*) => {
			let s = format!($f, $($s),*);
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "? {th_name}: {}", s)?;
				err.flush()?;
			}
			elatch = Some((Natural::ZERO, $c, s));
		};
	}

	macro_rules! debug {
    	($s:expr) => {
			if ll == LogLevel::Debug {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "DEBUG: {th_name}{}", $s)?;
				err.flush()?;
			}
		};
		($f:literal, $($s:expr),*) => {
			if ll == LogLevel::Debug {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "DEBUG: {th_name}{}", format!($f, $($s),*))?;
				err.flush()?;
			}
		};
	}

	let mut aptr: Vec<(Bound<usize>, Bound<usize>)> = Vec::new();

	let mut rptr: Option<Rational> = None;	//allow setting by a macro

	let mut rng = LazyCell::new(rng_os);

	let mut call: Vec<(Utf8Iter, Natural)> = vec![(start, Natural::const_from(1))];

	'mac: while let Some((mac, count)) = call.last_mut() {	//while call stack has contents, macro scope:
		let mut alt = false;

		let mut abuf: Vec<Value> = Vec::new();	//array input buffer
		let mut dest: Vec<NonNull<Vec<Value>>> = Vec::new();	//stack of destinations for new values, shadows mstk with the following macros
		/// push one value to main stack or array buffer
		macro_rules! push {
    		($v:expr) => {
				if let Some(p) = dest.last_mut() {
					unsafe {
						p.as_mut().push($v);
					}
				}
				else {
					st.mstk.push(Arc::new($v));
				}
			};
		}
		/// append values to main stack or array buffer
		macro_rules! append {
    		($v:expr) => {
				if let Some(p) = dest.last_mut() {
					unsafe {
						p.as_mut().append(&mut $v);
					}
				}
				else {
					for val in $v {
						st.mstk.push(Arc::new(val));
					}
				}
			};
		}

		'cmd: while let Some(b) = mac.next() {	//main parsing loop, single command scope:
			if let Some(rx) = kill {	//check kill signal
				match rx.try_recv() {
					Ok(()) => {	//killed by parent
						return Ok(Finished);
					},
					Err(TryRecvError::Empty) => {	//not killed
						//do nothing
					},
					Err(TryRecvError::Disconnected) => {	//parent should never disconnect
						unreachable!()
					}
				}
			}
			
			if let Some(e) = &mut elatch {	//advance error latch counter
				e.0 += Natural::ONE;
			}

			use Command::*;
			match byte_cmd(b) {
				Fn1(mon) => {
					if let Some(va) = st.mstk.pop() {
						match fns::exec1(mon, &va, alt) {
							Ok(vz) => {
								push!(vz);
							}
							Err(e) => {
								st.mstk.push(va);
								valerr!(b as char, "Function '{}': {}", b as char, e);
							}
						}
					}
					else {
						synerr!(b as char, "Function '{}' needs 1 argument, 0 given", b as char);
					}
				},
				Fn2(dya) => {
					if let Some(vb) = st.mstk.pop() {
						if let Some(va) = st.mstk.pop() {
							match fns::exec2(dya, &va, &vb, alt) {
								Ok(vz) => {
									push!(vz);
								}
								Err(e) => {
									st.mstk.push(va);
									st.mstk.push(vb);
									valerr!(b as char, "Function '{}': {}", b as char, e);
								}
							}
						}
						else {
							st.mstk.push(vb);
							synerr!(b as char, "Function '{}' needs 2 arguments, 1 given", b as char);
						}
					}
					else {
						synerr!(b as char, "Function '{}' needs 2 arguments, 0 given", b as char);
					}
				},
				Fn3(tri) => {
					if let Some(vc) = st.mstk.pop() {
						if let Some(vb) = st.mstk.pop() {
							if let Some(va) = st.mstk.pop() {
								match fns::exec3(tri, &va, &vb, &vc, alt) {
									Ok(vz) => {
										push!(vz);
									}
									Err(e) => {
										st.mstk.push(va);
										st.mstk.push(vb);
										st.mstk.push(vc);
										valerr!(b as char, "Function '{}': {}", b as char, e);
									}
								}
							}
							else {
								st.mstk.push(vb);
								st.mstk.push(vc);
								synerr!(b as char, "Function '{}' needs 3 arguments, 2 given", b as char);
							}
						}
						else {
							st.mstk.push(vc);
							synerr!(b as char, "Function '{}' needs 3 arguments, 1 given", b as char);
						}
					}
					else {
						synerr!(b as char, "Function '{}' needs 3 arguments, 0 given", b as char);
					}
				},
				Cmd(cmd) => {
					match cmd(st) {
						Ok(mut v) => {
							append!(v);
						}
						Err(e) => {
							valerr!(b as char, "Command '{}': {}", b as char, e);
						}
					}
				},
				CmdR(cmdr) => {
					let ri = if let Some(r) = rptr.take() {r}
					else {
						if matches!(mac.next().map(|b| {mac.back(); byte_cmd(b)}), None | Some(Space)) {
							synerr!(b as char, "Command '{}' needs a register index", b as char);
							continue 'cmd;
						}
						Rational::from(
							match mac.try_next_char() {
								Ok(c) => {c as u32},
								Err(e) => {
									*count = Natural::ZERO;
									synerr!('\0', "Aborting invalid macro: {}", e);
									break 'cmd;
								}
							}
						)
					};
					match cmdr(st, &ri) {
						Ok(mut v) => {
							append!(v);
						}
						Err(e) => {
							valerr!(b as char, "Command '{}': {}", b as char, e);
						}
					}
				},
				Special => {
					match b {
						b'`' => {	//alt prefix
							alt = true;
							continue 'cmd;	//force digraph
						},
						b'?' => {	//read line
							let prompt = if alt {
								if let Some(val) = st.mstk.pop() {
									match &*val {
										Value::S(s) => {
											s.clone()
										},
										_ => {
											let t = errors::TypeLabel::from(&*val);
											st.mstk.push(val);
											synerr!('Q', "Command '`?' needs a string, {} given", t);
											alt = false;
											continue 'cmd;
										}
									}
								}
								else {
									synerr!('?', "Command '`?' needs 1 argument, 0 given");
									continue 'cmd;
								}
							}
							else {
								String::new()
							};
							match io.lock().unwrap().0.read_line(&prompt) {
								Ok(s) => {
									push!(Value::S(s));
								},
								Err(e) => {
									match e.kind() {
										ErrorKind::Interrupted => {
											valerr!('?', "Command '?': Interrupted");
										},
										ErrorKind::UnexpectedEof => {
											push!(Value::S(String::new()));
										},
										_ => {
											return Err(e);
										}
									}
								}
							}
						},
						b'p' => {	//print top
							match (st.mstk.last(), alt) {
								(Some(val), false) => {
									outln!(&val.display(st.params.get_k(), st.params.get_o(), st.params.get_m()));
								},
								(Some(val), true) => {
									out!(&val.display(st.params.get_k(), st.params.get_o(), st.params.get_m()));
								},
								(None, false) => {
									synerr!('p', "Command 'p': Stack is empty");
								},
								(None, true) => {
									synerr!('p', "Command '`p': Stack is empty");
								}
							}
						},
						b'P' => {	//pop and print top
							match (st.mstk.pop(), alt) {
								(Some(val), false) => {
									outln!(&val.display(st.params.get_k(), st.params.get_o(), st.params.get_m()));
								},
								(Some(val), true) => {
									out!(&val.display(st.params.get_k(), st.params.get_o(), st.params.get_m()));
								},
								(None, false) => {
									synerr!('P', "Command 'P' needs 1 argument, 0 given");
								},
								(None, true) => {
									synerr!('P', "Command '`P' needs 1 argument, 0 given");
								}
							}
						},
						b'"' => {	//toggle pbuf
							if let Some(s) = pbuf.take() {	//close
								push!(Value::S(s));
							}
							else {	//open
								pbuf = Some(String::new());
							}
						},
						b'(' => {	//array input: open
							if !dest.is_empty() {	//first layer already exists
								abuf.push(Value::default());
							}
							let Value::A(new) = abuf.last_mut().unwrap() else { unreachable!() };
							dest.push(new.into());
						},
						b')' => {	//array input: close
							if dest.is_empty() {
								synerr!(')', "Mismatched closing ')'");
							}
							dest.pop().unwrap();
							if dest.is_empty() {	//completed
								st.mstk.push(Arc::new(Value::A(std::mem::take(&mut abuf))));
							}
						},
						b'q' => {
							let u = u8::wrapping_from(&Integer::rounding_from(rptr.unwrap_or_default(), RoundingMode::Down).0);
							return if alt {
								Ok(HardQuit(u.into()))
							}
							else {
								Ok(SoftQuit(u.into()))
							};
						},
						b'Q' => {
							if let Some(val) = st.mstk.pop() {
								match &*val {
									Value::N(r) => {
										if let Ok(u) = usize::try_from(r) {
											call.truncate(call.len().saturating_sub(u));
											if !dest.is_empty() {	//flush array buffer if not completed
												st.mstk.push(Arc::new(Value::A(std::mem::take(&mut abuf))));
											}
											continue 'mac;
										}
										else {
											let s = val.to_string();
											st.mstk.push(val);
											valerr!('Q', "Command 'Q': Cannot possibly break {} macros", s);
										}
									},
									_ => {
										let t = errors::TypeLabel::from(&*val);
										st.mstk.push(val);
										synerr!('Q', "Command 'Q' needs a number, {} given", t);
									}
								}
							}
							else {
								synerr!('Q', "Command 'Q' needs 1 argument, 0 given");
							}
						},
						b'a' => {	//array commands
							match mac.next() {
								Some(b) if !matches!(byte_cmd(b), Space) => {
									todo!()
								},
								Some(_) | None => {
									synerr!('a', "Incomplete array command: 'a'");
								}
							}
						},
						b'f' => {	//stack commands
							match mac.next() {
								Some(b) if !matches!(byte_cmd(b), Space) => {
									todo!()
								},
								Some(_) | None => {
									synerr!('f', "Incomplete stack command: 'f'");
								}
							}
						},
						b'N' => {
							match (st.mstk.pop(), alt) {
								(Some(val), false) => {	//get random natural
									match &*val {
										Value::N(r) => {
											match Natural::try_from(r) {
												Ok(n) => {
													push!(Value::N(Rational::from(get_random_natural_less_than(&mut rng, &n))));
												},
												_ => {
													st.mstk.push(val);
													valerr!('N', "Command 'N': Limit must be a natural number");
												}
											}
										},
										_ => {
											let t = errors::TypeLabel::from(&*val);
											st.mstk.push(val);
											synerr!('N', "Command 'N' needs a number, {} given", t);
										}
									}
								},
								(Some(val), true) => {	//seed rng
									match &*val {
										Value::N(r) => {
											match Integer::try_from(r) {
												Ok(Integer::NEGATIVE_ONE) => {	//return to os seed
													rng = LazyCell::new(rng_os);
												},
												Ok(i) if Natural::convertible_from(&i) => {	//custom seed
													let n= unsafe { Natural::try_from(i).unwrap_unchecked() };
													let mut bytes: Vec<u8> = PowerOf2DigitIterable::<u8>::power_of_2_digits(&n, 8).take(32).collect();
													bytes.resize(32, 0);
													*rng = rng_preset( unsafe { <[u8; 32]>::try_from(bytes).unwrap_unchecked() } );
												},
												_ => {
													st.mstk.push(val);
													valerr!('N', "Command '`N': Seed must be a natural number or `1");
												}
											}
										},
										_ => {
											let t = errors::TypeLabel::from(&*val);
											st.mstk.push(val);
											synerr!('N', "Command '`N' needs a number, {} given", t);
										}
									}
								},
								(None, false) => {
									synerr!('N', "Command 'N' needs 1 argument, 0 given");
								},
								(None, true) => {
									synerr!('N', "Command '`N' needs 1 argument, 0 given");
								}
							}
						},
						b'_' => {	//word commands
							let mut word = Vec::new();
							while let Some(b) = mac.next() {
								if matches!(byte_cmd(b), Space) {
									mac.back();
									break;
								}
								else {
									word.push(b);
								}
							}

							match &word[..] {	//word commands:
								b"restrict" => {
									restrict = true;
								},
								b"quiet" => {
									ll = LogLevel::Quiet;
								},
								b"error" => {
									ll = LogLevel::Normal;
								},
								b"debug" => {
									ll = LogLevel::Debug;
								},
								b"err" => {
									push!(Value::A(
										if let Some((n, c, s)) = elatch.take() {
											vec![
												Value::N(n.into()),
												Value::S(c.into()),
												Value::S(s)
											]
										}
										else { vec![] }
									));
								},
								b"th" => {
									push!(Value::S(th_name.clone()));
								},
								b"trim" => {
									st.trim();
								},
								b"clpar" => {
									st.params = ParamStk::default();
								},
								b"clall" => {
									*st = State::default();
								},
								_ => {
									#[cfg(feature = "no_os")]
									{
										synerr!('_', "Invalid word command: '{}'", string_or_bytes(&word));
									}
									#[cfg(not(feature = "no_os"))]
									{
										match (restrict, os::OS_CMDS.get(&word).copied()) {
											(false, Some(cmd)) => {
												match cmd(st) {
													Ok(mut v) => {
														append!(v);
													}
													Err(e) => {
														valerr!('_', "OS command '{}': {}", string_or_bytes(&word), e);
													}
												}
											},
											(true, Some(_)) => {
												valerr!('_', "OS command '{}' is disabled (restricted mode)", string_or_bytes(&word));
											},
											_ => {
												synerr!('_', "Invalid word command: '{}'", string_or_bytes(&word));
											}
										}
									}
								}
							}
						},
						_ => unreachable!()
					}
				},
				Lit => {
					todo!()
				},
				Space if b == b'#' => {	//line comment
					mac.find(|b| *b == b'\n');
				},
				Space => {
					//do nothing
				},
				Wrong => {
					mac.back();
					match mac.try_next_char() {
						Ok(c) => {
							synerr!(c, "Invalid command: {} (U+{:04X})", c, c as u32);
						},
						Err(e) => {
							*count = Natural::ZERO;
							synerr!('\0', "Aborting invalid macro: {}", e);
							break 'cmd;
						}
					}
				}
			}
			
			alt = false;	//reset after digraph
		}	//end of command scope

		if !dest.is_empty() {	//flush array buffer if not completed
			st.mstk.push(Arc::new(Value::A(std::mem::take(&mut abuf))));
		}

		if *count == Natural::ZERO {	//all repetitions finished
			call.pop();
		}
		else {	//more to go
			*count -= Natural::ONE;
			mac.rewind();
		}
	}	//end of macro scope
	
	Ok(Finished)
}