//! Executable CLI wrapper

use std::fs::{File, OpenOptions};
use std::io::{ErrorKind, Read, Seek, Write};
use std::process::ExitCode;
use std::sync::{Arc, Mutex};
use adc_lang::*;
use adc_lang::structs::{State, Utf8Iter};

const HELP: &str = "\
ADC: Array-oriented reimagining of dc, a terse RPN esolang (built-in executable CLI wrapper)

Full documentation and source code: https://github.com/43615/adc-lang

----------------------
COMMAND LINE ARGUMENTS
----------------------
Long and short flags are interchangeable. Short flags may not be merged: `-ie` is read as `-i e`, not `-i -e`.

	------------
	INSTRUCTIONS
	------------
	`-i|--inter <prompt>?` starts a looping interactive session. May use a custom prompt, the default is `> `.
		^C ends the session, ^D runs an empty string.
	`-e|--exec <macro>*` executes the argument(s) directly as a sequence of ADC commands.
		Ensure that any special characters in your shell are properly escaped.
	`-f|--file <path>*` executes the specified file(s) as a script, without creating or modifying it.
		If non-flag arguments are given (like `$ adc foo bar`), they are interpreted as file paths.

	Any amount of instructions may be provided, which will execute them in the given order. `-i` has to be exited manually to continue the sequence.
	The normal q command ends the current instruction, a hard quit (`q) also cancels all following instructions. Each quit updates the overall exit code.

	With no instructions or state flags (below), this CLI will attempt to find and run the file `~/.adcrc`, and then launch `-i`.

	----------------
	STATE MANAGEMENT
	----------------
	The whole interpreter state can be stored as a file using these flags (or in-language commands).
	State files are actually just ADC scripts that follow a standard format. Loading simply discards the current state and executes the script.

	`-s|--save <path>` saves the state to a file, creating or overwriting it.
	`-l|--load <path>` loads a state file, without creating or modifying it.

	These can be combined with instructions, which will perform them as part of the sequence.

	-----------
	IO/OS MODES
	-----------
	`-r|--restrict` disables all in-language commands that interact with the OS to protect against untrusted input. The CLI flags with file access remain functional.
		Disabling OS commands is also possible with the in-language command `_restrict` (one-way) or by building the crate with `-F no_os`.
	`-d|--debug`: Debug mode, logs every executed command to stderr.
	`-q|--quiet`: Quiet mode, suppresses all errors (disables stderr).
		Debug and Quiet are mutually exclusive. Also controllable with in-language commands, these just set the default for every instruction.
	`-t|--tee <path>` writes a copy of all IO streams to a file (opened in append mode). May only be used once.
	This CLI tries to open all files specified with `-f`/`-s`/`-l`/`-t` immediately, before any ADC code runs. Does not acquire exclusive locks, so beware of concurrent operations.

	-----
	OTHER
	-----
	`-h|--help` prints this message and exits, discarding all following flags.
	`-v|--version` prints the interpreter version and exits, ditto.\
";

fn main() -> ExitCode {
	fn syntax_error(s: String) -> ExitCode {
		eprintln!("!! {s}");
		1.into()
	}

	fn runtime_error(s: String) -> ExitCode {
		eprintln!("?? {s}");
		2.into()
	}
	
	enum Argument {
		Flag(char),
		Plain(String)
	}
	use Argument::*;
	const LONG: phf::Set<&'static [u8]> = phf::phf_set! {
		b"inter", b"exec", b"file", b"save", b"load", b"restrict", b"debug", b"quiet", b"tee", b"help", b"version"
	};
	debug_assert!(
		LONG.iter().map(|s| s[0]).enumerate().all(|(i, a)|
			LONG.iter().map(|s| s[0]).skip(i+1).all(|b| a!=b)
		),
		"long flags don't have unique first chars"
	);

	let mut args = Vec::new();

	//normalize and parse cli args
	for arg in std::env::args().skip(1) {
		if let Some(flag) = arg.strip_prefix('-') {
			if let Some(long) = flag.strip_prefix('-') {	//long-form flags:
				if long.is_empty() { return syntax_error("Empty flag: --".into()); }
				args.push(Flag(
					//reduce long-form flags
					if LONG.contains(long.as_bytes()) {
						long.as_bytes()[0] as char
					}
					else {
						return syntax_error(format!("Unrecognized flag: --{long}"));
					}
				));
			}
			else {	//short-form flags:
				let mut chars = flag.chars();
				//only take the first char as a flag
				let short = if let Some(c) = chars.next() {c}
					else { return syntax_error("Empty flag: -".into()); };
				args.push(Flag(short));
				//add the rest as a plain arg
				let plain: String = chars.collect();
				if !plain.is_empty() { args.push(Plain(plain)); }
			}
		}
		//plain arg, use as is
		else { args.push(Plain(arg)); }
	}

	#[derive(Debug)]
	enum Action {
		Inter(String),
		Macro(String),
		Script(File),
		Save(File),
		Load(File)
	}
	use Action::*;

	let mut acts = Vec::new();
	let mut restrict = false;
	let mut ll = LogLevel::Normal;

	fn try_reading_fd(name: &str) -> Result<File, ExitCode> {
		OpenOptions::new().read(true).open(name).map_err(|e|{
			runtime_error(format!("Can't open file '{name}' for reading: {e}"))
		})
	}

	fn try_appending_fd(name: &str) -> Result<File, ExitCode> {
		OpenOptions::new().append(true).create(true).open(name).map_err(|e| {
			runtime_error(format!("Can't open file '{name}' for appending: {e}"))
		})
	}

	fn try_writing_fd(name: &str) -> Result<File, ExitCode> {
		OpenOptions::new().write(true).create(true).truncate(false).open(name).map_err(|e| {
			runtime_error(format!("Can't open file '{name}' for writing: {e}"))
		})
	}

	let mut io: Option<Arc<Mutex<dyn IOStreams>>> = None;

	//parse tokens into actions + open files, already performs `--help` and `--version`
	let mut args = args.iter().peekable();
	let mut tee = false;
	while let Some(arg) = args.next() {
		match arg {
			Plain(filename) => {	//only possible before any flags
				acts.push(Script(
					match try_reading_fd(filename) {
						Ok(fd) => {fd}
						Err(e) => {return e;}
					}
				))
			}
			Flag('i') => {
				acts.push(Inter(
					if let Some(Plain(prompt)) = args.peek() {	//use custom prompt
						args.next();
						prompt.to_owned()
					}
					else { "> ".into() }	//or default prompt
				))
			}
			Flag('e') => {
				while let Some(Plain(mac)) = args.peek() {	//consume following args as expressions
					args.next();
					acts.push(Macro(mac.to_owned()));
				}
			}
			Flag('f') => {
				while let Some(Plain(path)) = args.peek() {	//consume following args as filenames
					args.next();
					acts.push(Script(
						match try_reading_fd(path) {
							Ok(fd) => {fd}
							Err(e) => {return e;}
						}
					))
				}
			}

			Flag('s') => {
				if let Some(Plain(path)) = args.peek() {	//consume one arg as filename
					args.next();
					if let Some(Plain(_)) = args.peek() {
						return syntax_error("Save option may only have one argument".into());
					}
					acts.push(Save(
						match try_writing_fd(path) {
							Ok(fd) => {fd}
							Err(e) => {return e;}
						}
					));
				}
				else {
					return syntax_error("Save option needs a file path".into());
				}
			}
			Flag('l') => {
				if let Some(Plain(path)) = args.peek() {	//consume one arg as filename
					args.next();
					if let Some(Plain(_)) = args.peek() {
						return syntax_error("Load option may only have one argument".into());
					}
					acts.push(Load(
						match try_reading_fd(path) {
							Ok(fd) => {fd}
							Err(e) => {return e;}
						}
					));
				}
				else {
					return syntax_error("Load option needs a file path".into());
				}
			}

			Flag('r') => { restrict = true; }
			Flag('d') => {
				if ll == LogLevel::Quiet { return syntax_error("Debug and Quiet modes are incompatible".into()); }
				ll = LogLevel::Debug;
			}
			Flag('q') => {
				if ll == LogLevel::Debug { return syntax_error("Debug and Quiet modes are incompatible".into()); }
				ll = LogLevel::Quiet;
			}
			Flag('t') if !tee => {
				tee = true;
				if let Some(Plain(path)) = args.peek() {	//consume one arg as filename
					args.next();
					if let Some(Plain(_)) = args.peek() {
						return syntax_error("Tee option may only have one argument".into());
					}
					match try_appending_fd(path) {
						Ok(fd) => {io = Some(Arc::new(Mutex::new(TeeProcessIO::new(fd))));}
						Err(e) => {return e;}
					}
				}
				else {
					return syntax_error("Tee option needs a file path".into());
				}
			}
			Flag('t') if tee => {
				return syntax_error("Tee option may only be used once".into());
			}

			Flag('h') => {
				println!("{HELP}");
				return 0.into();
			}
			Flag('v') => {
				println!("adc {}", option_env!("CARGO_PKG_VERSION").unwrap_or("(unknown version)"));
				return 0.into();
			}

			Flag(c) => {
				return syntax_error(format!("Unrecognized flag: -{c}"));
			}
		}
	}

	if acts.is_empty() {
		if let Some(mut pb) = std::env::home_dir() {	//try to read dotfile:
			pb.push(".adcrc");
			if let Ok(fd) = OpenOptions::new().read(true).open(pb) {
				acts.push(Script(fd));
			}
		}
		acts.push(Inter("> ".into()));	//default action
	}

	//init state structs
	let mut st = State::default();
	let io = io.unwrap_or_else(|| Arc::new(Mutex::new(ProcessIO::default())));
	let mut exit_code = 0;

	'act: for act in acts {
		use ExecResult::*;
		match act {
			Inter(prompt) => {
				'repl: loop {
					let mut lock = io.lock().unwrap();
					if let Err(c) = lock.print(prompt.clone()).map_err(|e| runtime_error(format!("Interactive mode IO error: {e}"))) {return c;}
					let mut input = lock.read_line().map_err(|e| e.kind());
					drop(lock);

					if input == Err(ErrorKind::UnexpectedEof) {input = Ok(String::new())};	//replace with empty string
					
					match input {
						Ok(line) => {
							let start = Utf8Iter::from(line.as_bytes());

							let res = unsafe { interpreter(&mut st, start, Arc::clone(&io), ll, None, restrict) };

							match res {
								Ok(Finished) => {continue 'repl;}
								Ok(SoftQuit(c)) => {exit_code = c; continue 'act;}
								Ok(HardQuit(c)) => {return c.into();}
								Ok(Killed) => unsafe {std::hint::unreachable_unchecked()}
								Err(e) => {return runtime_error(format!("Interpreter IO error: {e}"));}
							}
						},
						Err(ErrorKind::Interrupted) => {
							eprintln!("Interactive mode: Interrupted");
							continue 'act;
						},
						Err(e) => {
							return runtime_error(format!("Interactive mode IO error: {e}"));
						}
					}
				}
			}
			Macro(mac) => {
				let start = Utf8Iter::from(mac.as_bytes());

				let res = unsafe { interpreter(&mut st, start, Arc::clone(&io), ll, None, restrict) };

				match res {
					Ok(Finished) => {continue 'act;}
					Ok(SoftQuit(c)) => {exit_code = c; continue 'act;}
					Ok(HardQuit(c)) => {return c.into();}
					Ok(Killed) => unsafe {std::hint::unreachable_unchecked()}
					Err(e) => {return runtime_error(format!("Interpreter IO error: {e}"));}
				}
			}
			Script(mut fd) => {
				let mut script = Vec::new();
				if let Err(c) = fd.read_to_end(&mut script).map_err(|e| {runtime_error(format!("Can't read from script file: {e}"))}) {return c;}
				let start = Utf8Iter::from(script);

				let res = unsafe { interpreter(&mut st, start, Arc::clone(&io), ll, None, restrict) };

				match res {
					Ok(Finished) => {continue 'act;}
					Ok(SoftQuit(c)) => {exit_code = c; continue 'act;}
					Ok(HardQuit(c)) => {return c.into();}
					Ok(Killed) => unsafe {std::hint::unreachable_unchecked()}
					Err(e) => {return runtime_error(format!("Interpreter IO error: {e}"));}
				}
			}
			Save(mut fd) => {
				let map = |e: std::io::Error| {runtime_error(format!("Can't write to state file: {e}"))};
				if let Err(c) = fd.set_len(0).map_err(map) {return c;}
				if let Err(c) = fd.rewind().map_err(map) {return c;}
				if let Err(c) = fd.write_all(st.to_string().as_bytes()).map_err(map) {return c;}
			}
			Load(mut fd) => {
				//state files are just ADC scripts
				let mut st_script = Vec::new();
				if let Err(c) = fd.read_to_end(&mut st_script).map_err(|e| {runtime_error(format!("Can't read from state file: {e}"))}) {return c;}
				if !st_script.starts_with(&STATE_FILE_HEADER) {return runtime_error("Invalid state file".into());}
				let start = Utf8Iter::from(st_script);

				let mut nst = State::default();

				let res = interpreter_simple(&mut nst, start, None);	//delegate to normal interpreter

				if !matches!(res, Ok(Finished)) {return runtime_error("Invalid state file".into());}	//state files don't quit
				
				st.replace_vals(nst);
			}
		}
	}

	exit_code.into()
}