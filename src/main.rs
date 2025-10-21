//! Executable CLI wrapper

use std::convert::Into;
use std::fs::{File, OpenOptions};
use std::io::{ErrorKind, Read, Seek, Write};
use std::process::ExitCode;
use std::sync::Mutex;
use adc_lang::*;
use adc_lang::structs::{State, Utf8Iter};

const HELP: &str = r#"
ADC: Array-oriented reimagining of dc, a terse RPN esolang
	Terminal-based wrapper executable

Full documentation and source code: https://github.com/43615/adc-lang

COMMAND LINE ARGUMENTS
----------------------
Long and short flags are interchangeable. Short flags may not be concatenated: `-ie` is read as `-i e`, not `-i -e`.

	INSTRUCTIONS
	------------
	`-i|--inter <prompt>?` starts an interactive (REPL) session. May use a custom prompt, the default is `> `.
		This is the default if no instructions are given. ^C and ^D end the session.
	`-e|--exec <macro>*` executes the argument(s) directly as a sequence of ADC commands.
		Ensure that characters with special uses in your shell's syntax are ignored/escaped properly.
	`-f|--file <path>*` executes the specified file(s) as a script, without creating or modifying it.
		If non-flag arguments are given (like `$ adc foo bar`), this is implied.
	Multiple of these can be used together, which will execute them sequentially. `-i` has to be exited manually to continue the sequence.

	STATE MANAGEMENT
	----------------
	The whole interpreter state can be stored as a file using these flags (or in-language commands).
	State files are actually just ADC scripts that follow a standard format. The "load" option executes the script and discards the current state.
	`-s|--save <path>` saves the state to a file, creating or overwriting it.
	`-l|--load <path>` loads a state file, without creating or modifying it.
	These are also chainable with the instruction flags, which will perform them as part of the sequence.
	This executable tries to open all files specified with `-f`/`-s`/`-l` immediately, before any ADC code runs. It does not acquire exclusive locks, so beware of data races.

	MODES
	-----
	`-r|--restrict`: Disables all in-language commands that interact with the OS to protect against untrusted input. Other command line flags are unaffected.
		Disabling OS commands is also possible with the in-language command `_restrict` (one-way) or the `no_os` crate feature.
	`-d|--debug`: Debug mode, logs every executed command to stderr.
	`-q|--quiet`: Quiet mode, suppresses all errors (disables stderr).
	Debug and Quiet are mutually exclusive. The error display mode is controllable with in-language commands, these options just set the default.

	OTHER
	-----
	`-h|--help` prints this message and exits, discarding all following flags.
	`-v|--version` prints the interpreter version and exits, ditto.
"#;

fn main() -> ExitCode {
	fn syntax_error(s: String) -> ExitCode {
		eprintln!("!! {s}");
		1.into()
	}

	fn runtime_error(s: String) -> ExitCode {
		eprintln!("!? {s}");
		2.into()
	}

	#[derive(Debug)]
	enum Argument {
		Flag(char),
		Plain(String)
	}
	use Argument::*;

	let mut args = Vec::new();

	//normalize and parse cli args into tokens
	for arg in std::env::args().skip(1) {
		if let Some(flag) = arg.strip_prefix('-') {
			if let Some(long) = flag.strip_prefix('-') {	//long-form flags:
				if long.is_empty() { return syntax_error("Empty flag: --".into()); }
				args.push(Flag(
					//reduce long-form flags
					match long {
						"inter" => {'i'}
						"exec" => {'e'}
						"file" => {'f'}
						"save" => {'s'}
						"load" => {'l'}
						"restrict" => {'r'}
						"debug" => {'d'}
						"quiet" => {'q'}
						"help" => {'h'}
						"version" => {'v'}
						_ => {
							return syntax_error(format!("Unrecognized flag: --{long}"));
						}
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
	let mut strict = false;
	let mut ll = LogLevel::Normal;

	fn try_reading_fd(name: &str) -> Result<File, ExitCode> {
		OpenOptions::new().read(true).open(name).map_err(|e|{
			runtime_error(format!("Can't open file '{name}' for reading: {e}"))
		})
	}

	fn try_writing_fd(name: &str) -> Result<File, ExitCode> {
		OpenOptions::new().write(true).create(true).truncate(false).open(name).map_err(|e| {
			runtime_error(format!("Can't open file '{name}' for writing: {e}"))
		})
	}

	//parse tokens into actions + open files, already performs `--help` and `--version`
	let mut args = args.iter().peekable();
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
				while let Some(Plain(file)) = args.peek() {	//consume following args as filenames
					args.next();
					acts.push(Script(
						match try_reading_fd(file) {
							Ok(fd) => {fd}
							Err(e) => {return e;}
						}
					))
				}
			}

			Flag('s') => {
				if let Some(Plain(file)) = args.peek() {	//consume one arg as filename
					args.next();
					if let Some(Plain(_)) = args.peek() {
						return syntax_error("Save option may only have one argument".into());
					}
					acts.push(Save(
						match try_writing_fd(file) {
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
				if let Some(Plain(file)) = args.peek() {	//consume one arg as filename
					args.next();
					if let Some(Plain(_)) = args.peek() {
						return syntax_error("Load option may only have one argument".into());
					}
					acts.push(Load(
						match try_reading_fd(file) {
							Ok(fd) => {fd}
							Err(e) => {return e;}
						}
					));
				}
				else {
					return syntax_error("Load option needs a file path".into());
				}
			}

			Flag('r') => { strict = true; }
			Flag('d') => {
				if ll == LogLevel::Quiet { return syntax_error("Debug and Quiet modes are incompatible".into()); }
				ll = LogLevel::Debug;
			}
			Flag('q') => {
				if ll == LogLevel::Debug { return syntax_error("Debug and Quiet modes are incompatible".into()); }
				ll = LogLevel::Quiet;
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

	if acts.is_empty() { acts.push(Inter("> ".into())); }	//default action

	//init state structs
	let mut st = State::default();
	let mut io = Mutex::new(IOStreams::process());
	let mut exit_code = 0.into();

	'act: for act in acts {
		use ExecResult::*;
		match act {
			Inter(prompt) => {
				'repl: loop {
					if let Err(c) = io.get_mut().unwrap().1.write_all(prompt.as_bytes()).map_err(|e| runtime_error(format!("Interactive mode IO error: {e}"))) {return c;}
					if let Err(c) = io.get_mut().unwrap().1.flush().map_err(|e| runtime_error(format!("Interactive mode IO error: {e}"))) {return c;}
					match io.get_mut().unwrap().0.read_line() {
						Ok(line) => {
							let start = Utf8Iter::from(line.as_bytes());

							let res = interpreter(&mut st, start, &io, ll, None, strict);

							match res {
								Ok(Finished) => {continue 'repl;}
								Ok(SoftQuit(c)) => {exit_code = c; continue 'act;}
								Ok(HardQuit(c)) => {return c;}
								Err(e) => {return runtime_error(format!("Interpreter IO error: {e}"));}
							}
						}
						Err(e) => {
							match e.kind() {
								ErrorKind::Interrupted => {
									eprintln!("Interactive mode: Interrupted");
									continue 'act;
								},
								ErrorKind::UnexpectedEof => {
									eprintln!("Interactive mode: EOF");
									continue 'act;
								}
								_ => {
									return runtime_error(format!("Interactive mode IO error: {e}"));
								}
							}
						}
					}
				}
			}
			Macro(mac) => {
				let start = Utf8Iter::from(mac.as_bytes());

				let res = interpreter(&mut st, start, &io, ll, None, strict);

				match res {
					Ok(Finished) => {continue 'act;}
					Ok(SoftQuit(c)) => {exit_code = c; continue 'act;}
					Ok(HardQuit(c)) => {return c;}
					Err(e) => {return runtime_error(format!("Interpreter IO error: {e}"));}
				}
			}
			Script(mut fd) => {
				let mut script = Vec::new();
				if let Err(c) = fd.read_to_end(&mut script).map_err(|e| {runtime_error(format!("Can't read from script file: {e}"))}) {return c;}
				let start = Utf8Iter::from(script);

				let res = interpreter(&mut st, start, &io, ll, None, strict);

				match res {
					Ok(Finished) => {continue 'act;}
					Ok(SoftQuit(c)) => {exit_code = c; continue 'act;}
					Ok(HardQuit(c)) => {return c;}
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
				let no_io = Mutex::new(IOStreams::empty());

				let res = interpreter(&mut nst, start, &no_io, LogLevel::Quiet, None, true);	//delegate to normal interpreter

				if res.unwrap() != Finished {return runtime_error("Invalid state file".into());}	//state files don't quit
				
				st = nst;
			}
		}
	}

	exit_code
}