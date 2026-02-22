//! ADC interpreter API. Many safe items are public for manual access.
//! 
//! The core function is [`interpreter`], see its documentation as a starting point.

pub mod structs;
use structs::*;

pub(crate) mod fns;

pub(crate) mod cmds;

pub(crate) mod errors;

pub(crate) mod conv;

pub(crate) mod num;

#[cfg(not(feature = "no_os"))]
mod os;

use std::io::{Write, BufRead, ErrorKind};
use std::ops::Neg;
use std::panic::catch_unwind;
use std::ptr::NonNull;
use std::str::FromStr;
use std::sync::{Arc, Mutex, mpsc::{Receiver, TryRecvError, RecvTimeoutError}};
use linefeed::{DefaultTerminal, Interface};
use bitvec::prelude::*;
use malachite::{Natural, Integer, Rational};
use malachite::base::num::arithmetic::traits::{NegAssign, Pow};
use malachite::base::num::basic::traits::{NegativeOne, Zero, One};
use malachite::base::num::conversion::traits::{ConvertibleFrom, PowerOf2DigitIterable, RoundingFrom, WrappingFrom};
use malachite::base::num::random::RandomPrimitiveInts;
use malachite::base::rational_sequences::RationalSequence;
use malachite::base::rounding_modes::RoundingMode;
use crate::errors::TypeLabel;


/// Added at the start of saved state files
pub const STATE_FILE_HEADER: [u8;20] = *b"# ADC state file v1\n";

struct LineEditor(Interface<DefaultTerminal>);

/// Generic input stream adapter trait, used for adding a proper line editor.
///
/// Comes with a generic implementation for all [`BufRead`] types.
pub trait ReadLine {
	/// This is called once by every execution of the `?` command within ADC.
	///
	/// [`ErrorKind::Interrupted`] cancels `?` with an error, [`ErrorKind::UnexpectedEof`] makes an empty string, other [`ErrorKind`]s are returned early from the interpreter.
	fn read_line(&mut self) -> std::io::Result<String>;

	/// If [`Self`] implements a history, clear it.
	fn clear_history(&mut self);
}
impl<T: BufRead> ReadLine for T {
	fn read_line(&mut self) -> std::io::Result<String> {
		let mut buf = String::new();
		self.read_line(&mut buf)?;
		Ok(buf)
	}

	fn clear_history(&mut self) {
		// no history, do nothing
	}
}
impl ReadLine for LineEditor {
	fn read_line(&mut self) -> std::io::Result<String> {
		use linefeed::{ReadResult, Signal};
		match self.0.read_line() {
			Ok(ReadResult::Input(s)) => {
				self.0.add_history_unique(s.clone());
				Ok(s)
			},
			Ok(ReadResult::Eof) => {Err(ErrorKind::UnexpectedEof.into())},
			Ok(ReadResult::Signal(sig)) => {
				self.0.cancel_read_line()?;
				match sig {
					Signal::Break | Signal::Interrupt | Signal::Quit => {Err(ErrorKind::Interrupted.into())},
					Signal::Continue => {Err(std::io::Error::other("Unhandled SIGCONT"))},
					Signal::Suspend => {Err(std::io::Error::other("Unhandled SIGTSTP"))},
					Signal::Resize => {Err(std::io::Error::other("Unhandled window resize"))},
				}
			},
			Err(e) => {Err(e)}
		}
	}

	fn clear_history(&mut self) {
		self.0.clear_history();
	}
}

fn input_stream() -> Box<dyn ReadLine + Send> {
	use linefeed::Signal::*;
	match Interface::new("") {	//fails when pipes are used
		Ok(iface) => {
			//these do not take &mut self, shrug
			iface.set_report_signal(Break, true);
			iface.set_report_signal(Interrupt, true);
			iface.set_report_signal(Quit, true);
			Box::new(LineEditor(iface))
		},
		Err(_) => {	//fall back to plain stdin
			Box::new(std::io::BufReader::new(std::io::stdin()))
		}
	}
}

/// Bundle of standard IO streams, generic interface to support custom IO wrappers
pub struct IOStreams (
	/// Input
	pub Box<dyn ReadLine + Send>,
	/// Output
	pub Box<dyn Write + Send>,
	/// Error
	pub Box<dyn Write + Send>
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

	/// Use IO streams of the process (stdin, stdout, stderr), with extra [line editor](linefeed) on stdin
	pub fn process() -> Self {
		Self (
			input_stream(),
			Box::new(std::io::stdout()),
			Box::new(std::io::stderr())
		)
	}
}

lazy_static::lazy_static! {
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

#[derive(Default, Debug, Clone, Copy, Hash)]
enum Command {
	///monadic pure function
	Fn1(fns::Mon),

	///dyadic pure function
	Fn2(fns::Dya),
	
	///triadic pure function
	Fn3(fns::Tri),

	///impure command
	Cmd(cmds::Cmd),

	///really impure (macros, IO, OS...)
	Exec,

	///impure with register access
	ExecR,

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
		Space,		Fn1(neg),	Exec,		Space,		Wrong,		Fn2(modu),	Wrong,		Lit,		Exec,		Exec,		Fn2(mul),	Fn2(add),	Wrong,		Fn2(sub),	Lit,		Fn2(div),

		//0			1			2			3			4			5			6			7			8			9			:			;			<			=			>			?
		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Lit,		Exec,		Wrong,		Fn2(lt),	Fn2(eq),	Fn2(gt),	Exec,

		//@			A			B			C			D			E			F			G			H			I			J			K			L			M			N			O
		Lit,		Wrong,		Wrong,		Cmd(cln),	Exec,		Wrong,		Lit,		Fn2(logb),	Wrong,		Cmd(gi),	ExecR,		Cmd(gk),	ExecR,		Cmd(gm),	Exec,		Cmd(go),

		//P			Q			R			S			T			U			V			W			X			Y			Z			[			\			]			^			_
		Exec,		Exec,		Exec,		ExecR,		Lit,		Wrong,		Fn2(root),	Exec,		ExecR,		Wrong,		ExecR,		Lit,		Wrong,		Wrong,		Fn2(pow),	Exec,

		//`			a			b			c			d			e			f			g			h			i			j			k			l			m			n			o
		Exec,		Exec,		Wrong,		Cmd(cls),	Exec,		Wrong,		Exec,		Fn1(log),	Wrong,		Cmd(si),	ExecR,		Cmd(sk),	ExecR,		Cmd(sm),	Fn1(fac),	Cmd(so),

		//p			q			r			s			t			u			v			w			x			y			z			{			|			}			~			DEL
		Exec,		Exec,		Cmd(rev),	ExecR,		Fn2(trig),	Wrong,		Fn1(sqrt),	Exec,		Exec,		Wrong,		Fn1(disc),	Cmd(cbo),	Fn3(bar),	Cmd(cbc),	Fn2(euc),	Wrong,

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

const fn upper_hex_to_nibble(b: u8) -> Option<u8> {
	match b {
		b'0'..=b'9' => Some(unsafe{b.unchecked_sub(0x30)}),	//SAFETY: underflow is impossible
		b'A'..=b'F' => Some(unsafe{b.unchecked_sub(0x37)}),
		_ => None
	}
}

const fn mixed_ascii_to_digit(b: u8) -> Option<u8> {
	match b {
		b'0'..=b'9' => Some(unsafe{b.unchecked_sub(0x30)}),	//SAFETY: underflow is impossible
		b'A'..=b'Z' => Some(unsafe{b.unchecked_sub(0x37)}),
		b'a'..=b'z' => Some(unsafe{b.unchecked_sub(0x57)}),
		_ => None
	}
}

/// How much information to output on stderr
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogLevel {
	/// Only output error messages
	Normal,
	/// Report every command (without values)
	Debug,
	/// No error messages, stderr disabled
	Quiet
}

/// Results of running [`interpreter`], wrappers should handle these differently
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[must_use] pub enum ExecResult {
	/// All commands completed
	Finished,

	/// `q` command (early exit from current set of commands)
	SoftQuit(u8),

	/// `` `q `` command (early exit from entire interpreter, if applicable)
	HardQuit(u8),

	/// Received kill signal while executing commands
	Killed
}

/// Safe wrapper around [`interpreter`] with no IO or OS features.
///
/// If built with the `no_os` feature, this is equivalent to [`interpreter_no_io`] and should be preferred.
pub fn interpreter_simple(
	st: &mut State,
	start: Utf8Iter,
	kill: Option<&Receiver<()>>
) -> std::io::Result<ExecResult>
{
	let no_io = Arc::new(Mutex::new(IOStreams::empty()));
	interpreter_no_os(st, start, no_io, LogLevel::Quiet, kill)
}

/// Safe wrapper around [`interpreter`] with no OS features.
///
/// If built with the `no_os` feature, this is equivalent to [`interpreter`] and should be preferred.
pub fn interpreter_no_os(
	st: &mut State,
	start: Utf8Iter,
	io: Arc<Mutex<IOStreams>>,
	ll: LogLevel,
	kill: Option<&Receiver<()>>,
) -> std::io::Result<ExecResult>
{
	//SAFETY: restrict = true
	unsafe { interpreter(st, start, io, ll, kill, true) }
}

/// IO-less wrapper around [`interpreter`].
///
/// # Safety
/// See [`interpreter#safety`].
pub unsafe fn interpreter_no_io(
	st: &mut State,
	start: Utf8Iter,
	kill: Option<&Receiver<()>>,
	restrict: bool
) -> std::io::Result<ExecResult>
{
	let no_io = Arc::new(Mutex::new(IOStreams::empty()));
	unsafe { interpreter(st, start, no_io, LogLevel::Quiet, kill, restrict) }
}

/// Interpreter entry point, executes ADC commands to modify state.
/// 
/// This function and its variants are also available as methods of [`State`] if such syntax is preferred.
///
/// # Arguments
/// - `st`: State struct to work on, modified in-place
/// - `start`: Initial commands to run
/// - `io`: Bundle of IO stream handles
///   - `io.0`: Input, read by ? one line at a time
///   - `io.1`: Output, written to by printing commands
///   - `io.2`: Error messages, one per line
/// - `ll`: Level of verbosity for `io.2`
/// - `kill`: Optional kill signal, receiving `()` terminates the interpreter immediately.
///   - `Some(_)` will also enable `restrict`, as it's unsafe or impossible to kill the thread while performing some OS interactions.
/// - `restrict`: Restricted mode switch, enable for untrusted input. If `false`, the interpreter may read/write files and execute OS commands, subject to any OS-level permissions.
///
/// # Errors
/// Any IO errors (except those specified [here](ReadLine::read_line)) that arise when accessing the [`IOStreams`] are returned early, aborting the interpreter.
///
/// This will result in an incomplete, although internally consistent, [`State`]. Keep that possibility to a minimum when preparing custom [`IOStreams`].
///
/// # Safety
/// If built without the `no_os` feature (default), passing `kill = None` and `restrict = false` enables OS interactions, some of which are fundamentally unsound in multithreaded contexts.
///
/// Simultaneously/asynchronously executing multiple instances of this function with said arguments may cause *Undefined Behavior*, potentially including permanent corruption of OS resources.
///
/// If OS features are not desired, [`interpreter_no_os`] and [`interpreter_simple`] provide safe wrappers.
pub unsafe fn interpreter(
	st: &mut State,
	start: Utf8Iter,
	io: Arc<Mutex<IOStreams>>,
	mut ll: LogLevel,
	kill: Option<&Receiver<()>>,
	#[cfg_attr(feature = "no_os", expect(unused_variables))]
	mut restrict: bool
) -> std::io::Result<ExecResult>
{
	use ExecResult::*;

	let th_name = if kill.is_some() {	//if running in a child thread
		#[cfg_attr(feature = "no_os", expect(unused_assignments))] { restrict = true; }
		std::thread::current().name().unwrap().to_owned()
	}
	else {
		String::new()
	};

	let mut pbuf: Option<String> = None;	//print-to-string buffer

	let mut elatch: Option<(Natural, char, String)> = None;

	macro_rules! synerr {
		($c:expr, $s:expr) => {
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "! {th_name}{}: {}", $c, $s)?;
				err.flush()?;
				//drop lock
			}
			elatch = Some((Natural::ZERO, $c, $s.into()));
		};
    	($c:expr, $f:literal, $($s:expr),*) => {
			let s = format!($f, $($s),*);
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "! {th_name}{}: {}", $c, s)?;
				err.flush()?;
				//drop lock
			}
			elatch = Some((Natural::ZERO, $c, s));
		};
	}

	macro_rules! valerr {
    	($c:expr, $s:expr) => {
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "? {th_name}{}: {}", $c, $s)?;
				err.flush()?;
				//drop lock
			}
			elatch = Some((Natural::ZERO, $c, $s.into()));
		};
		($c:expr, $f:literal, $($s:expr),*) => {
			let s = format!($f, $($s),*);
			if ll != LogLevel::Quiet {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "? {th_name}{}: {}", $c, s)?;
				err.flush()?;
				//drop lock
			}
			elatch = Some((Natural::ZERO, $c, s));
		};
	}

	macro_rules! debug {
    	($s:expr) => {
			if ll == LogLevel::Debug {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "\tDEBUG: {th_name}{}", $s)?;
				err.flush()?;
				//drop lock
			}
		};
		($f:literal, $($s:expr),*) => {
			if ll == LogLevel::Debug {
				let err = &mut io.lock().unwrap().2;
				writeln!(err, "\tDEBUG: {th_name}{}", format!($f, $($s),*))?;
				err.flush()?;
				//drop lock
			}
		};
	}

	let mut rptr: Option<Rational> = None;	//allow setting by a macro

	let mut rng: Option<RandomPrimitiveInts<u64>> = None;

	let mut call: Vec<(Utf8Iter, Natural)> = vec![(start, Natural::const_from(1))];

	'mac: while let Some((mac, count)) = call.last_mut() {	//while call stack has contents, macro scope:
		*count -= Natural::ONE;
		let mut alt = false;

		let mut abuf: Vec<Value> = Vec::new();	//array input buffer
		let mut dest: Vec<NonNull<Vec<Value>>> = Vec::new();	//stack of destinations for new values, shadows mstk with the following macros
		/// push one value to main stack or array buffer
		macro_rules! push {
    		($v:expr) => {
				if let Some(p) = dest.last_mut() {
					unsafe {
						p.as_mut().push($v);	//SAFETY: `NonNull`s point to nested arrays in abuf, only one is accessed at a time
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
						p.as_mut().append(&mut $v);	//SAFETY: `NonNull`s point to nested arrays in abuf, only one is accessed at a time
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
						#[expect(unused_assignments)]
						for s in st.regs.end_threads(true) {	//propagate to children
							valerr!('j', s);
						}
						return Ok(Killed);
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
						debug!("Monadic {}{} with {}", if alt {"alt-"} else {""}, b as char, TypeLabel::from(&*va));
						match catch_unwind(|| fns::exec1(mon, &va, alt)) {
							Ok(Ok(vz)) => {
								push!(vz);
							},
							Ok(Err(e)) => {
								st.mstk.push(va);
								valerr!(b as char, e.to_string());
							},
							Err(cause) => {
								st.mstk.push(va);
								valerr!(b as char, "Caught function panic: {:?}", cause);
							}
						}
					}
					else {
						synerr!(b as char, "Expected 1 argument, 0 given");
					}
				},
				Fn2(dya) => {
					if let Some(vb) = st.mstk.pop() {
						if let Some(va) = st.mstk.pop() {
							debug!("Dyadic {}{} with ({}, {})", if alt {"alt-"} else {""}, b as char, TypeLabel::from(&*va), TypeLabel::from(&*vb));
							match catch_unwind(|| fns::exec2(dya, &va, &vb, alt)) {
								Ok(Ok(vz)) => {
									push!(vz);
								},
								Ok(Err(e)) => {
									st.mstk.push(va);
									st.mstk.push(vb);
									valerr!(b as char, e.to_string());
								},
								Err(cause) => {
									st.mstk.push(va);
									st.mstk.push(vb);
									valerr!(b as char, "Caught function panic: {:?}", cause);
								}
							}
						}
						else {
							st.mstk.push(vb);
							synerr!(b as char, "Expected 2 arguments, 1 given");
						}
					}
					else {
						synerr!(b as char, "Expected 2 arguments, 0 given");
					}
				},
				Fn3(tri) => {
					if let Some(vc) = st.mstk.pop() {
						if let Some(vb) = st.mstk.pop() {
							if let Some(va) = st.mstk.pop() {
								debug!("Triadic {}{} with ({}, {}, {})", if alt {"alt-"} else {""}, b as char, TypeLabel::from(&*va), TypeLabel::from(&*vb), TypeLabel::from(&*vc));
								match catch_unwind(|| fns::exec3(tri, &va, &vb, &vc, alt)) {
									Ok(Ok(vz)) => {
										push!(vz);
									},
									Ok(Err(e)) => {
										st.mstk.push(va);
										st.mstk.push(vb);
										st.mstk.push(vc);
										valerr!(b as char, e.to_string());
									},
									Err(cause) => {
										st.mstk.push(va);
										st.mstk.push(vb);
										st.mstk.push(vc);
										valerr!(b as char, "Caught function panic: {:?}", cause);
									}
								}
							}
							else {
								st.mstk.push(vb);
								st.mstk.push(vc);
								synerr!(b as char, "Expected 3 arguments, 2 given");
							}
						}
						else {
							st.mstk.push(vc);
							synerr!(b as char, "Expected 3 arguments, 1 given");
						}
					}
					else {
						synerr!(b as char, "Expected 3 arguments, 0 given");
					}
				},
				Cmd(cmd) => {
					debug!("Impure command {}", b as char);
					match cmd(st) {
						Ok(mut v) => {
							append!(v);
						}
						Err(e) => {
							if let Some(se) = e.strip_suffix('!') {
								synerr!(b as char, se);
							}
							else {
								valerr!(b as char, e);
							}
						}
					}
				},
				Exec => {
					debug!("Command {}{}", if alt {"alt-"} else {""}, b as char);
					match b {
						b'`' => {	//alt prefix
							alt = true;
							continue 'cmd;	//force digraph
						},
						b':' if !alt => {	//set register pointer
							if let Some(va) = st.mstk.pop() {
								if let Value::N(r) = &*va {
									rptr = Some(r.clone());
								}
								else {
									let ta = TypeLabel::from(&*va);
									st.mstk.push(va);
									synerr!(':', "Expected a number, {} given", ta);
								}
							}
							else {
								synerr!(':', "Expected 1 argument, 0 given");
							}
						},
						b':' if alt => {	//get register pointer
							push!(
								if let Some(ri) = rptr.take() {
									Value::N(ri)
								}
								else {
									Value::A(vec![])
								}
							);
						},
						b'd' => {
							if let Some(v) = st.mstk.last() {
								if let Some(p) = dest.last_mut() {	//manual shadowed push
									unsafe {
										p.as_mut().push((**v).clone());	//SAFETY: `NonNull`s point to nested arrays in abuf, only one is accessed at a time
									}
								}
								else {
									st.mstk.push(Arc::clone(v));
								}
							}
							else {
								synerr!('d', "Stack is empty");
							}
						},
						b'D' => {
							if let Some(va) = st.mstk.pop() {
								if let Value::N(r) = &*va {
									match usize::try_from(r) {
										Ok(0) => {},	//no-op
										Ok(u) => {
											if let Some(from) = st.mstk.len().checked_sub(u) {
												if let Some(p) = dest.last_mut() {	//manual shadowed append
													for v in &st.mstk[from..] {
														unsafe {
															p.as_mut().push((**v).clone());	//SAFETY: `NonNull`s point to nested arrays in abuf, only one is accessed at a time
														}
													}
												}
												else {
													st.mstk.extend_from_within(from..);
												}
											}
											else {
												st.mstk.push(va);
												valerr!('D', "Can't duplicate {} values, stack depth is {}", u, st.mstk.len() - 1);
											}
										}
										Err(_) => {
											let vs = va.to_string();
											st.mstk.push(va);
											valerr!('D', "Can't possibly duplicate {} values", vs);
										}
									}
								}
								else {
									let ta = TypeLabel::from(&*va);
									st.mstk.push(va);
									synerr!('D', "Expected a number, {} given", ta);
								}
							}
							else {
								synerr!('D', "Expected 1 argument, 0 given");
							}
						},
						b'R' => {	//rotate
							if let Some(va) = st.mstk.pop() {
								if let Value::N(r) = &*va {
									match usize::try_from(r) {
										Ok(0) => {},	//no-op
										Ok(u) => {
											if let Some(from) = st.mstk.len().checked_sub(u) {
												if alt {st.mstk[from..].rotate_left(1);}
												else {st.mstk[from..].rotate_right(1);}
											}
											else {
												st.mstk.push(va);
												valerr!('R', "Can't rotate {} values, stack depth is {}", u, st.mstk.len() - 1);
											}
										}
										Err(_) => {
											let vs = va.to_string();
											st.mstk.push(va);
											valerr!('R', "Can't possibly rotate {} values", vs);
										}
									}
								}
								else {
									let ta = TypeLabel::from(&*va);
									st.mstk.push(va);
									synerr!('R', "Expected a number, {} given", ta);
								}
							}
							else {
								synerr!('R', "Expected 1 argument, 0 given");
							}
						},
						b'?' => {	//read line
							let res = {
								let inp = &mut io.lock().unwrap().0;
								inp.read_line()
								//drop lock
							};
							match res {
								Ok(s) => {
									push!(Value::S(s));
								},
								Err(e) => {
									match e.kind() {
										ErrorKind::Interrupted => {
											valerr!('?', "Interrupted");
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
						b'p' => {	//println top
							if let Some(va) = st.mstk.pop() {
								let vs = va.display(st.params.get_k(), st.params.get_o(), st.params.get_m(), alt);
								if let Some(s) = &mut pbuf {
									s.push_str(&vs);
									s.push('\n');
								}
								else {
									let out = &mut io.lock().unwrap().1;
									writeln!(out, "{}", vs)?;
									out.flush()?;
									//drop lock
								}
							}
							else {
								synerr!('p', "Expected 1 argument, 0 given");
							}
						},
						b'P' => {	//print top
							if let Some(va) = st.mstk.pop() {
								let vs = va.display(st.params.get_k(), st.params.get_o(), st.params.get_m(), alt);
								if let Some(s) = &mut pbuf {
									s.push_str(&vs);
								}
								else {
									let out = &mut io.lock().unwrap().1;
									write!(out, "{}", vs)?;
									out.flush()?;
									//drop lock
								}
							}
							else {
								synerr!('P', "Expected 1 argument, 0 given");
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
							let nn = if let Some(p) = dest.last_mut() { unsafe {	//SAFETY: dest is only mutated here, logic is sound
								p.as_mut().push(Value::A(Vec::new()));	//create nested A
								let Value::A(new) = &mut p.as_mut().last_mut().unwrap_unchecked() else { std::hint::unreachable_unchecked() };	//and get reference to it
								NonNull::from(new)
							}}
							else {
								NonNull::from(&mut abuf)	//abuf itself as first layer
							};
							dest.push(nn);
						},
						b')' => {	//array input: close
							if dest.pop().is_some() {
								if dest.is_empty() {	//completed
									st.mstk.push(Arc::new(Value::A(std::mem::take(&mut abuf))));	//commit to stack
								}
							}
							else {
								synerr!(')', "Mismatched closing ')'");
							}
						},
						b'x' => {
							if let Some(top) = st.mstk.pop() {
								let sec = st.mstk.pop();
								match Utf8Iter::try_macros(&top, sec.as_deref()) {
									Ok((stk, ret)) => {
										if let Some(sec) = sec && ret {	//sec was not used, return
											st.mstk.push(sec);
										}

										if mac.is_finished() && *count == Natural::ZERO {	//tail call optimization: discard current macro if finished
											call.pop();
										}

										call.extend(stk.into_iter().rev());
										continue 'mac;
									},
									Err(e) => {
										if let Some(sec) = sec {st.mstk.push(sec);}
										st.mstk.push(top);
										synerr!('x', "{}", e);
									}
								}
							}
							else {
								synerr!('x', "Expected 1 or 2 arguments, 0 given");
							}
						},
						b'q' => {
							let u = u8::wrapping_from(&Integer::rounding_from(rptr.unwrap_or_default(), RoundingMode::Down).0);
							return if alt {
								Ok(HardQuit(u))
							}
							else {
								Ok(SoftQuit(u))
							};
						},
						b'Q' => {
							if let Some(va) = st.mstk.pop() {
								match &*va {
									Value::N(r) => {
										if let Ok(u) = usize::try_from(r) {
											call.truncate(call.len().saturating_sub(u));
											if !dest.is_empty() {	//flush array buffer if not completed
												st.mstk.push(Arc::new(Value::A(std::mem::take(&mut abuf))));
												dest.clear();	//SAFETY: remove leftover dangling references
											}
											continue 'mac;
										}
										else {
											let vs = va.to_string();
											st.mstk.push(va);
											valerr!('Q', "Cannot possibly break {} macros", vs);
										}
									},
									_ => {
										let ta = TypeLabel::from(&*va);
										st.mstk.push(va);
										synerr!('Q', "Expected a number, {} given", ta);
									}
								}
							}
							else {
								synerr!('Q', "Expected 1 argument, 0 given");
							}
						},
						b'a' => {	//array commands
							match mac.next() {
								Some(b) if !matches!(byte_cmd(b), Space) => {
									match b {
										b'a' => {
											todo!()
										},
										_ => {
											synerr!('a', "Invalid array command 'a{}'", b as char);
										}
									}
								},
								Some(_) | None => {
									synerr!('a', "Incomplete array command 'a'");
								}
							}
						},
						b'f' => {	//stack commands
							match mac.next() {
								Some(b) if !matches!(byte_cmd(b), Space) => {
									match b {
										b'z' => {	//stack depth
											push!(Value::N(st.mstk.len().into()));
										},
										b'r' => {	//reverse stack
											st.mstk.reverse();
										},
										b'R' => {	//reverse part of stack
											if let Some(va) = st.mstk.pop() {
												if let Value::N(r) = &*va {
													match usize::try_from(r) {
														Ok(0) => {},	//no-op
														Ok(u) => {
															if let Some(from) = st.mstk.len().checked_sub(u) {
																st.mstk[from..].reverse();
															}
															else {
																st.mstk.push(va);
																valerr!('f', "Can't reverse {} values, stack depth is {}", u, st.mstk.len() - 1);
															}
														}
														Err(_) => {
															let vs = va.to_string();
															st.mstk.push(va);
															valerr!('f', "Can't possibly reverse {} values", vs);
														}
													}
												}
												else {
													let ta = TypeLabel::from(&*va);
													st.mstk.push(va);
													synerr!('f', "Expected a number, {} given", ta);
												}
											}
											else {
												synerr!('f', "Expected 1 argument, 0 given");
											}
										},
										b'f' => {	//swap with reg
											let ri = if let Some(r) = rptr.take() {r}
											else {
												if matches!(mac.next().map(|b| {mac.back(); byte_cmd(b)}), None | Some(Space)) {
													synerr!('f', "No register index");
													alt = false;
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
											let reg = st.regs.get_mut(&ri);
											std::mem::swap(&mut st.mstk, &mut reg.v);
										},
										b'p' => {	//print stack
											for v in &st.mstk {
												let vs = v.display(st.params.get_k(), st.params.get_o(), st.params.get_m(), alt);
												if let Some(s) = &mut pbuf {
													s.push_str(&vs);
													s.push('\n');
												}
												else {
													let out = &mut io.lock().unwrap().1;
													writeln!(out, "{}", vs)?;
													out.flush()?;
												}
											}
										},
										_ => {
											synerr!('f', "Invalid stack command 'f{}'", b as char);
										}
									}
								},
								Some(_) | None => {
									synerr!('f', "Incomplete stack command 'f'");
								}
							}
						},
						b'N' => {
							match (st.mstk.pop(), alt) {
								(Some(va), false) => {	//get random natural
									match &*va {
										Value::N(r) => {
											match Natural::try_from(r) {
												Ok(n) => {
													push!(Value::N(Rational::from(
														malachite::natural::random::get_random_natural_less_than(rng.get_or_insert_with(rng_os), &n)
													)));
												},
												_ => {
													st.mstk.push(va);
													valerr!('N', "Limit must be a natural number");
												}
											}
										},
										_ => {
											let ta = TypeLabel::from(&*va);
											st.mstk.push(va);
											synerr!('N', "Expected a number, {} given", ta);
										}
									}
								}
								(Some(va), true) => {	//seed rng
									match &*va {
										Value::N(r) => {
											match Integer::try_from(r) {
												Ok(Integer::NEGATIVE_ONE) => {	//return to os seed (lazy init)
													rng = None;
												},
												Ok(i) if Natural::convertible_from(&i) => {	//custom seed
													let n= unsafe { Natural::try_from(i).unwrap_unchecked() };	//SAFETY: just checked
													let mut bytes: Vec<u8> = PowerOf2DigitIterable::<u8>::power_of_2_digits(&n, 8).take(32).collect();
													bytes.resize(32, 0);
													rng = Some(rng_preset(
														unsafe { <[u8; 32]>::try_from(bytes).unwrap_unchecked() }
													));
												},
												_ => {
													st.mstk.push(va);
													valerr!('N', "Seed must be a natural number or `1");
												}
											}
										},
										_ => {
											let ta = TypeLabel::from(&*va);
											st.mstk.push(va);
											synerr!('N', "Expected a number, {} given", ta);
										}
									}
								}
								(None, _) => {
									synerr!('N', "Expected 1 argument, 0 given");
								}
							}
						},
						b'w' => {	//wait
							if let Some(va) = st.mstk.pop() {
								if let Value::N(r) = &*va {
									if let Some(dur) = u128::try_from(r).ok().and_then(|ns| {
										(ns <= std::time::Duration::MAX.as_nanos()).then(|| std::time::Duration::from_nanos_u128(ns))
									}) {
										if let Some(rx) = kill {
											match rx.recv_timeout(dur) {
												Ok(()) => {	//killed by parent
													#[expect(unused_assignments)]
													for s in st.regs.end_threads(true) {	//propagate to children
														valerr!('j', s);
													}
													return Ok(Killed);
												},
												Err(RecvTimeoutError::Timeout) => {	//wait completed, not killed
													//do nothing
												},
												Err(RecvTimeoutError::Disconnected) => {	//parent should never disconnect
													unreachable!()
												}
											}
										}
										else {
											std::thread::sleep(dur);	//no kill receiver, just sleep
										}
									}
									else {
										let vs = va.to_string();
										st.mstk.push(va);
										valerr!('w', "Can't possibly wait {} ns", vs);
									}
								}
								else {
									let ta = TypeLabel::from(&*va);
									st.mstk.push(va);
									synerr!('w', "Expected a number, {} given", ta);
								}
							}
							else {
								synerr!('w', "Expected 1 argument, 0 given");
							}
						},
						b'W' => {
							push!(Value::N(
								match std::time::SystemTime::UNIX_EPOCH.elapsed() {
									Ok(dur) => {
										Rational::from(dur.as_nanos())
									},
									Err(e) => {
										Rational::from(e.duration().as_nanos()).neg()
									}
								}
							));
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
									#[cfg_attr(feature = "no_os", expect(unused_assignments))] { restrict = true; }
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
								b"joinall" => {
									for s in st.regs.end_threads(false) {
										valerr!('j', s);
									}
								},
								b"killall" => {
									for s in st.regs.end_threads(true) {
										valerr!('j', s);
									}
								},
								b"trim" => {
									st.trim();
									RE_CACHE.clear();
								},
								b"dedup" => {
									st.dedup();
								},
								b"clhist" => {
									io.lock().unwrap().0.clear_history();
								},
								b"clpar" => {
									st.params = ParamStk::default();
								},
								b"clall" => {
									st.clear_vals();
									RE_CACHE.clear();
								},
								_ => {
									#[cfg(feature = "no_os")]
									{
										synerr!('_', "Invalid word command '{}'", string_or_bytes(&word));
									}
									#[cfg(not(feature = "no_os"))]
									{
										match (restrict, os::OS_CMDS.get(&word).copied()) {
											(false, Some(oscmd)) => {
												let res;
												unsafe {
													if os::ACCESS {	//breach of interpreter safety contract
														panic!("Failsafe panic: Attempted to execute multiple OS commands simultaneously. See documentation of `interpreter` function for safe usage.");
													}
													else {
														os::ACCESS = true;
													}

													res = oscmd(st);

													os::ACCESS = false;
												}
												match res {
													Ok(mut v) => {
														append!(v);
													}
													Err(e) => {
														if let Some(se) = e.strip_suffix('!') {
															synerr!('_', "OS command '{}': {}", string_or_bytes(&word), se);
														}
														else {
															valerr!('_', "OS command '{}': {}", string_or_bytes(&word), e);
														}
													}
												}
											}
											(true, Some(_)) => {
												synerr!('_', "OS command '{}' is disabled (restricted mode)", string_or_bytes(&word));
											},
											_ => {
												synerr!('_', "Invalid word command '{}'", string_or_bytes(&word));
											}
										}
									}
								}
							}
						},
						_ => unreachable!()
					}
				},
				ExecR => {
					let ri = if let Some(r) = rptr.take() {r}
					else {
						if matches!(mac.next().map(|b| {mac.back(); byte_cmd(b)}), None | Some(Space)) {
							synerr!(b as char, "No register index");
							alt = false;
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
					debug!("Register command {}{}", if alt {"alt-"} else {""}, b as char);
					match b {
						b'Z' => {	//stack depth
							push!(Value::N(
								st.regs.try_get(&ri).map(|reg| reg.v.len().into()).unwrap_or_default()
							));
						},
						b's' => {
							if let Some(va) = st.mstk.pop() {
								let reg = st.regs.get_mut(&ri);
								if let Some(rv) = reg.v.last_mut() {
									*rv = va;
								}
								else {
									reg.v.push(va);
								}
							}
							else {
								synerr!('s', "Stack is empty");
							}
						},
						b'S' => {
							if let Some(va) = st.mstk.pop() {
								st.regs.get_mut(&ri).v.push(va);
							}
							else {
								synerr!('S', "Stack is empty");
							}
						},
						b'l' => {
							if let Some(rv) = st.regs.try_get(&ri).and_then(|reg| reg.v.last()) {
								if let Some(p) = dest.last_mut() {	//manual shadowed push
									unsafe {
										p.as_mut().push((**rv).clone());	//SAFETY: `NonNull`s point to nested arrays in abuf, only one is accessed at a time
									}
								}
								else {
									st.mstk.push(Arc::clone(rv));
								}
							}
							else {
								synerr!('l', "Register {} is empty", reg_index_nice(&ri));
							}
						},
						b'L' => {
							if let Some(rv) = st.regs.try_get_mut(&ri).and_then(|reg| reg.v.pop()) {
								if let Some(p) = dest.last_mut() {	//manual shadowed push
									unsafe {
										p.as_mut().push((*rv).clone());	//SAFETY: `NonNull`s point to nested arrays in abuf, only one is accessed at a time
									}
								}
								else {
									st.mstk.push(Arc::clone(&rv));
								}
							}
							else {
								synerr!('L', "Register {} is empty", reg_index_nice(&ri));
							}
						},
						b'X' => {
							if let Some(true) = st.regs.try_get(&ri).map(|reg| reg.th.is_some()) {
								valerr!('X', "Register {} is already running a thread", reg_index_nice(&ri));
							}
							else if let Some(va) = st.mstk.pop() {
								if let Value::S(sa) = &*va {
									let th_start = sa.to_owned().into();
									let (ktx, krx) = std::sync::mpsc::channel::<()>();
									let (jtx, jrx) = std::sync::mpsc::channel::<()>();
									let tb = std::thread::Builder::new().name(format!("{th_name}{}: ", reg_index_nice(&ri)));
									let mut th_st = if alt { st.clone() } else { State::default() };
									let th_io = Arc::clone(&io);

									match tb.spawn(move || {
										////////////////////////////////
										let th_res = interpreter_no_os(&mut th_st, th_start, th_io, ll, Some(&krx));

										let _ = jrx.recv();	//wait for join signal
										th_st.regs.end_threads(	//join children
											th_res.is_err()	//kill on io error
											||
											match krx.try_recv() {	//check for unprocessed kill signal
												Ok(()) => { true },
												Err(TryRecvError::Empty) => { false },
												Err(TryRecvError::Disconnected) => { unreachable!() }	//parent should never disconnect
											}
										);

										(th_st.mstk, th_res)
										////////////////////////////////
									}) {
										Ok(jh) => {
											st.regs.get_mut(&ri).th = Some((jh, ktx, jtx));
										},
										Err(e) => {
											st.mstk.push(va);
											valerr!('X', "Can't spawn child thread: {}", e);
										}
									}
								}
								else {
									let ta = TypeLabel::from(&*va);
									st.mstk.push(va);
									synerr!('X', "Expected a string, {} given", ta);
								}
							}
							else {
								synerr!('X', "Expected 1 argument, 0 given");
							}
						},
						b'j' => {
							let ri_nice = reg_index_nice(&ri);
							if let Some(reg) = st.regs.try_get_mut(&ri) && let Some((jh, ktx, jtx)) = reg.th.take() {
								if alt {
									ktx.send(()).unwrap_or_else(|_| panic!("Thread {} panicked, terminating!", ri_nice));
								}
								jtx.send(()).unwrap_or_else(|_| panic!("Thread {} panicked, terminating!", ri_nice));
								match jh.join() {
									Ok(mut tr) => {
										match tr.1 {
											Err(e) => {
												let s = format!("IO error in thread {}: {}", ri_nice, e);
												eprintln!("? {th_name}j: {}", s);	//io.2 might be unusable, fall back to fresh stderr
												elatch = Some((Natural::ZERO, 'j', s));
											},
											Ok(SoftQuit(c)) if c != 0 => {
												valerr!('j', "Thread {} quit with code {}", ri_nice, c);
											},
											Ok(HardQuit(c)) if c != 0 => {
												valerr!('j', "Thread {} hard-quit with code {}", ri_nice, c);
											},
											Ok(Killed) => {
												valerr!('j', "Thread {} was killed", ri_nice);
											},
											_ => {}	//finished or quit with 0
										}

										reg.v.append(&mut tr.0);
									},
									Err(e) => {
										std::panic::resume_unwind(e);
									}
								}
							}
							else {
								valerr!('j', "Register {} is not running a thread", ri_nice);
							}
						},
						b'J' => {
							if let Some(Some((jh, _, _))) = st.regs.try_get(&ri).map(|reg| &reg.th) {
								let mut bz = BitVec::new();
								bz.push(jh.is_finished());
								push!(Value::B(bz));
							}
							else {
								valerr!('J', "Register {} is not running a thread", reg_index_nice(&ri));
							}
						},
						_ => unreachable!()
					}
				},
				Lit => {
					match b {
						b'T' | b'F' => {	//booleans
							debug!("Boolean literal");
							let mut bits = BitVec::new();
							bits.push(b == b'T');
							while let Some(b) = mac.next() {
								match b {
									b'T' => {bits.push(true);},
									b'F' => {bits.push(false);},
									_ => {
										mac.back();
										break;
									}
								}
							}
							push!(Value::B(bits));
						},
						b'\'' | b'0'..=b'9' | b'.' | b'@' => {	//numbers
							debug!("Number literal");
							let mut ipart = Vec::new();
							let mut fpart = Vec::new();
							let mut rpart = Vec::new();
							let mut get_epart = true;
							let mut exp = None;
							let mut discard = false;
							let mut ibase = st.params.get_i().clone();

							match (b == b'\'', ibase > Natural::const_from(36)) {
								(false, high_base) => {	//plain
									mac.back();
									ibase = if high_base {Natural::const_from(10)} else {ibase};	//if plain but I>36, interpret mantissa with I=10
									while let Some(ib) = mac.next() {	//integer part
										let id = ib.wrapping_sub(0x30);
										match id {
											0..=9 if id < ibase => {ipart.push(Natural::from(id));},
											0..=9 => {
												synerr!('\'', "Digit {} is too high for base {}", id, ibase);
												discard = true;
											},
											_ => {
												mac.back();
												break;
											}
										}
									}
									match mac.next() {
										Some(b'.') => {	//fractional part
											let mut recur = false;	//recurring digits started
											while let Some(fb) = mac.next() {
												let fd = fb.wrapping_sub(0x30);
												match fd {
													0x30 if !recur => {recur = true;},	//b'`' == 0x60
													0..=9 if !recur && fd < ibase => {fpart.push(Natural::from(fd));},
													0..=9 if recur && fd < ibase => {rpart.push(Natural::from(fd));},
													0..=9 => {
														synerr!('\'', "Digit {} is too high for base {}", fd, ibase);
														discard = true;
													},
													_ => {
														mac.back();
														break;
													}
												}
											}
										},
										Some(_) => {mac.back();},
										None => {}
									}
								}
								(true, false) => {	//prefixed
									while let Some(ib) = mac.next() {	//integer part
										if let Some(id) = mixed_ascii_to_digit(ib) {
											if id < ibase {ipart.push(Natural::from(id));}
											else {
												synerr!('\'', "Digit {} is too high for base {}", id, ibase);
												discard = true;
											}
										}
										else {
											mac.back();
											break;
										}
									}
									match mac.next() {
										Some(b'.') => {	//fractional part
											let mut recur = false;	//recurring digits started
											while let Some(fb) = mac.next() {
												if let Some(fd) = mixed_ascii_to_digit(fb) {
													if fd < ibase {
														if !recur {fpart.push(Natural::from(fd));}
														else {rpart.push(Natural::from(fd));}
													}
													else {
														synerr!('\'', "Digit {} is too high for base {}", fd, ibase);
														discard = true;
													}
												}
												else if !recur && fb == b'`' {recur = true;}
												else {
													mac.back();
													break;
												}
											}
										},
										Some(_) => {mac.back();},
										None => {}
									}
								},
								(true, true) => {	//enclosed
									get_epart = false;
									let ns= mac.by_ref().take_while(|b| *b != b'\'').collect::<Vec<u8>>();
									if ns.is_empty() {
										synerr!('\'', "Empty any-base number");
										alt = false;
										continue 'cmd;
									}
									for nc in ns.iter() {
										match nc {
											b' ' | b'.' | b'0'..=b'9' | b'@' | b'`' => {}	//fine
											wrong => {
												synerr!('\'', "Invalid character in any-base number: {}", string_or_bytes(&[*wrong]));
												alt = false;
												continue 'cmd;
											}
										}
									}
									let mut ms = Vec::new();
									match ns.split(|b| *b == b'@').collect::<Vec<&[u8]>>()[..] {
										[mpart] => {	//only mantissa
											ms = mpart.to_vec();
										},
										[mpart, epart] => {	//mantissa and exponent
											ms = mpart.to_vec();
											let mut es = epart.to_vec();
											if let Some(first) = es.first_mut() && *first == b'`' { *first = b'-'; }
											match String::from_utf8(es) {
												Ok(es) => {
													match es.parse::<i64>() {
														Ok(i) => { exp = Some(i); },
														Err(e) => {
															use std::num::IntErrorKind::*;
															match e.kind() {
																Empty => { exp = Some(0); },
																InvalidDigit => {
																	valerr!('\'', "Invalid exponent: {}", es);
																	alt = false;
																	continue 'cmd;
																},
																PosOverflow | NegOverflow => {
																	valerr!('\'', "Exponent {} is unrepresentable", es);
																	alt = false;
																	continue 'cmd;
																},
																_ => { unreachable!() }
															}
														}
													}
												},
												_ => { unreachable!() }
											}
										},
										ref v => {
											synerr!('\'', "{} exponent signs (@) in any-base number", v.len() - 1);
											drop(ms);
											alt = false;
											continue 'cmd;
										}
									}
									let mut is = Vec::new();
									let mut frs = Vec::new();
									match ms.split(|b| *b == b'.').collect::<Vec<&[u8]>>()[..] {
										[ipart] => {
											is = ipart.to_vec();
										},
										[ipart, fpart] => {
											is = ipart.to_vec();
											frs = fpart.to_vec();
										},
										ref v => {
											synerr!('\'', "{} fractional points (.) in any-base number", v.len() - 1);
											drop(is);
											alt = false;
											continue 'cmd;
										}
									}
									if is.contains(&b'`') {
										synerr!('\'', "Unexpected negative sign (`) in any-base number");
										alt = false;
										continue 'cmd;
									}
									let mut fs = Vec::new();
									let mut rs = Vec::new();
									match frs.split(|b| *b == b'`').collect::<Vec<&[u8]>>()[..] {
										[fpart] => {
											fs = fpart.to_vec();
										},
										[fpart, rpart] => {
											fs = fpart.to_vec();
											rs = rpart.to_vec();
										},
										ref v => {
											synerr!('\'', "{} recurring marks (`) in any-base number", v.len() - 1);
											drop(fs);
											alt = false;
											continue 'cmd;
										}
									}
									if !is.is_empty() { for id in is.split(|b| *b == b' ') {
										let id = str::from_utf8(id).unwrap();
										ipart.push(Natural::from_str(id).unwrap());
									}}
									if !fs.is_empty() { for fd in fs.split(|b| *b == b' ') {
										let fd = str::from_utf8(fd).unwrap();
										fpart.push(Natural::from_str(fd).unwrap());
									}}
									if !rs.is_empty() { for rd in rs.split(|b| *b == b' ') {
										let rd = str::from_utf8(rd).unwrap();
										rpart.push(Natural::from_str(rd).unwrap());
									}}
									for d in ipart.iter().chain(fpart.iter()).chain(rpart.iter()) {
										if *d >= ibase {
											synerr!('\'', "Digit {} is too high for base {}", d, ibase);
											alt = false;
											continue 'cmd;
										}
									}
								}
							}

							let m_empty = ipart.is_empty() && fpart.is_empty() && rpart.is_empty();	//simpler const zero, also keep track of emptiness
							let mut r;
							if m_empty {
								r = Rational::ZERO;
							}
							else {
								ipart.reverse();	//malachite needs reverse order
								r = Rational::from_digits(&ibase, ipart, RationalSequence::from_vecs(fpart, rpart));
								if alt {r.neg_assign();}	//negative sign was set using alt
							}

							if get_epart {
								match mac.next() {
									Some(b'@') => {    //exponent part
										let mut es = String::new();
										let mut eneg = false;    //negative sign occurred
										while let Some(eb) = mac.next() {
											match eb {
												b'`' if !eneg => { es.push('-'); }
												b'0'..=b'9' => { es.push(eb as char); }
												_ => {
													mac.back();
													break;
												}
											}
											eneg = true;    //only allow on first char
										}
										if es.is_empty() { es.push('0'); }
										if m_empty { r = Rational::ONE; }    //only exponent part present
										if let Ok(i) = es.parse::<i64>() {
											r *= Rational::from(st.params.get_i()).pow(i);    //apply exponent, get original ibase
										}
										else {
											valerr!('\'', "Exponent {} is unrepresentable", es);
											discard = true;
										}
									}
									Some(_) => { mac.back(); }
									None => {}
								}
							}
							else if let Some(i) = exp {
       							if m_empty { r = Rational::ONE; }    //only exponent part present
       							r *= Rational::from(ibase).pow(i);    //apply exponent
							}

							if !discard {
								push!(Value::N(r));
							}
						},
						b'[' => {	//strings
							debug!("String literal");
							let mut bytes = Vec::new();
							let mut discard = false;
							let mut nest = 1usize;
							while let Some(b) = mac.next() {
								match b {
									b'[' => {
										nest = unsafe { nest.unchecked_add(1) };
										bytes.push(b'[');
									},
									b']' => {
										nest = unsafe { nest.unchecked_sub(1) };
										if nest == 0 {
											break;
										}
										else {
											bytes.push(b']');
										}
									},
									b'\\' => {	//character escapes
										match mac.next() {
											Some(b'a') => {bytes.push(0x07);},		//bell
											Some(b'b') => {bytes.push(0x08);},		//backspace
											Some(b't') => {bytes.push(0x09);},		//horizontal tab
											Some(b'n') => {bytes.push(0x0A);},		//line feed
											Some(b'v') => {bytes.push(0x0B);},		//vertical tab
											Some(b'f') => {bytes.push(0x0C);},		//form feed
											Some(b'r') => {bytes.push(0x0D);},		//carriage return
											Some(b'e') => {bytes.push(0x1B);},		//escape
											Some(b'[') => {bytes.push(b'[');},		//literal opening bracket
											Some(b']') => {bytes.push(b']');},		//literal closing bracket
											Some(b'\\') => {bytes.push(b'\\');},	//literal backslash
											Some(b0) => {							//other escapes
												if let Some(high) = upper_hex_to_nibble(b0) {	//byte literal
													if let Some(b1) = mac.next() {
														if let Some(low) = upper_hex_to_nibble(b1) {
															bytes.push(high << 4 | low);
														}
														else {
															synerr!('[', "Invalid byte escape: \\{}{}", b0 as char, b1 as char);
															discard = true;
														}
													}
													else {
														synerr!('[', "Incomplete byte escape: \\{}", b0 as char);
														discard = true;
													}
												}
												else {	//wrong escape
													mac.back();
													match mac.try_next_char() {
														Ok(c) => {
															synerr!('[', "Invalid character escape: \\{} (U+{:04X})", c, c as u32);
															discard = true;
														},
														Err(e) => {
															*count = Natural::ZERO;
															synerr!('\0', "Aborting invalid macro: {}", e);
															break 'cmd;
														}
													}
												}
											},
											None => {
												synerr!('[', "Incomplete character escape: \\");
												discard = true;
											}
										}
									},
									_ => {
										bytes.push(b);
									}
								}
							}
							if !discard {
								match String::try_from(Utf8Iter::from(bytes)) {
									Ok(s) => {
										push!(Value::S(s));
									},
									Err(e) => {
										synerr!('[', "Invalid string: {}", e);
									}
								}
							}
						},
						_ => unreachable!()
					}
				},
				Space if b == b'#' => {	//line comment
					debug!("Line comment");
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
			dest.clear();	//SAFETY: remove leftover dangling references
		}

		if *count == Natural::ZERO {	//all repetitions finished
			call.pop();
		}
		else {	//more to go
			mac.rewind();
		}
	}	//end of macro scope
	
	Ok(Finished)
}