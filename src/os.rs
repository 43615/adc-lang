//! OS-interfacing commands, only safe to use in a single thread

use std::cell::LazyCell;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::process::{Command, Stdio, Child};
use bitvec::prelude::*;
// use crate::cmds::*;
use crate::errors::TypeLabel;
use crate::structs::{Value, State};

/// OS command
pub(crate) type OsCmd = unsafe fn(&mut State) -> Result<Vec<Value>, String>;
/// Function template for OS command
macro_rules! oscmd {
    ($name:ident, $st:ident $block:block) => {
		pub(crate) unsafe fn $name($st: &mut State) -> Result<Vec<Value>, String> {
			$block
		}
	}
}


pub(crate) const OS_CMDS: phf::Map<&[u8], OsCmd> = phf::phf_map! {
	//static env queries
	b"osarch" => osarch,
	b"osfamily" => osfamily,
	b"osname" => osname,
	b"pid" => pid,

	//file operations
	b"read" => read,
	b"append" => append,
	b"write" => write,
	b"load" => load,
	b"save" => save,

	//env operations
	b"setvar" => setvar,
	b"getvar" => getvar,
	b"setdir" => setdir,
	b"getdir" => getdir,
	b"homedir" => homedir,
	b"tempdir" => tempdir,

	//child processes
	b"run" => run,
	b"spawn" => spawn,
	b"wait" => wait,
	b"exited" => exited,
	b"kill" => kill
};

/// Persistent map of child process handles
///
/// SAFETY: Module is private, interpreter has a safety contract that forbids access from multiple threads.
static mut PIDS: LazyCell<HashMap<u32, Child>> = LazyCell::new(HashMap::new);

oscmd!(osarch, _st {
	Ok(vec![Value::S(std::env::consts::ARCH.into())])
});

oscmd!(osfamily, _st {
	Ok(vec![Value::S(std::env::consts::FAMILY.into())])
});

oscmd!(osname, _st {
	Ok(vec![Value::S(std::env::consts::OS.into())])
});

oscmd!(pid, _st {
	Ok(vec![Value::N(std::process::id().into())])
});

fn try_reading_fd(name: &str) -> Result<File, String> {
	OpenOptions::new().read(true).open(name).map_err(|e| format!("Can't open file '{name}' for reading: {e}"))
}

fn try_appending_fd(name: &str) -> Result<File, String> {
	OpenOptions::new().append(true).create(true).open(name).map_err(|e| format!("Can't open file '{name}' for appending: {e}"))
}

fn try_writing_fd(name: &str) -> Result<File, String> {
	OpenOptions::new().write(true).create(true).truncate(true).open(name).map_err(|e| format!("Can't open file '{name}' for writing: {e}"))
}

oscmd!(read, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::S(name) = &*va {
			let mut v = Vec::new();
			match try_reading_fd(name) {
				Ok(mut fd) => {
					match fd.read_to_end(&mut v) {
						Ok(_) => {
							Ok(vec![Value::S(String::from_utf8_lossy(&v).into())])
						},
						Err(e) => {
							let name = name.to_owned();
							st.mstk.push(va);
							Err(format!("Can't read from file '{name}': {e}"))
						}
					}
				},
				Err(es) => {
					st.mstk.push(va);
					Err(es)
				}
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

oscmd!(append, st {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::S(s), Value::S(name)) => {
					match try_appending_fd(name) {
						Ok(mut fd) => {
							match fd.write_all(s.as_bytes()) {
								Ok(()) => {
									Ok(vec![])
								},
								Err(e) => {
									let name = name.to_owned();
									st.mstk.push(va);
									st.mstk.push(vb);
									Err(format!("Can't append to file '{name}': {e}"))
								}
							}
						},
						Err(es) => {
							st.mstk.push(va);
							st.mstk.push(vb);
							Err(es)
						}
					}
				},
				_ => {
					let ta = TypeLabel::from(&*va);
					let tb = TypeLabel::from(&*vb);
					st.mstk.push(va);
					st.mstk.push(vb);
					Err(format!("Expected two strings, {} and {} given!", ta, tb))
				}
			}
		}
		else {
			st.mstk.push(vb);
			Err("Expected 2 arguments, 1 given!".into())
		}
	}
	else {
		Err("Expected 2 arguments, 0 given!".into())
	}
});

oscmd!(write, st {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::S(s), Value::S(name)) => {
					match try_writing_fd(name) {
						Ok(mut fd) => {
							match fd.write_all(s.as_bytes()) {
								Ok(()) => { Ok(vec![]) },
								Err(e) => {
									let name = name.to_owned();
									st.mstk.push(va);
									st.mstk.push(vb);
									Err(format!("Can't write to file '{name}': {e}"))
								}
							}
						},
						Err(es) => {
							st.mstk.push(va);
							st.mstk.push(vb);
							Err(es)
						}
					}
				},
				_ => {
					let ta = TypeLabel::from(&*va);
					let tb = TypeLabel::from(&*vb);
					st.mstk.push(va);
					st.mstk.push(vb);
					Err(format!("Expected two strings, {} and {} given!", ta, tb))
				}
			}
		}
		else {
			st.mstk.push(vb);
			Err("Expected 2 arguments, 1 given!".into())
		}
	}
	else {
		Err("Expected 2 arguments, 0 given!".into())
	}
});

oscmd!(load, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::S(name) = &*va {
			let mut v = Vec::new();
			match try_reading_fd(name) {
				Ok(mut fd) => {
					match fd.read_to_end(&mut v) {
						Ok(_) => {
							if !v.starts_with(&crate::STATE_FILE_HEADER) {
								let name = name.to_owned();
								st.mstk.push(va);
								Err(format!("State file '{name}' is invalid"))
							}
							else {
								let st_mac = crate::structs::Utf8Iter::from(v);
								let mut new_st = State::default();
								let no_io = std::sync::Arc::new(std::sync::Mutex::new(crate::IOStreams::empty()));
								
								let res = crate::interpreter_no_os(&mut new_st, st_mac, no_io, crate::LogLevel::Quiet, None);	//one recursive call as a treat
								
								if !matches!(res, Ok(crate::ExecResult::Finished)) {
									let name = name.to_owned();
									st.mstk.push(va);
									Err(format!("State file '{name}' is invalid"))
								}
								else {
									st.replace_vals(new_st);
									Ok(vec![])
								}
							}
						},
						Err(e) => {
							let name = name.to_owned();
							st.mstk.push(va);
							Err(format!("Can't read from state file '{name}': {e}"))
						}
					}
				},
				Err(es) => {
					st.mstk.push(va);
					Err(es)
				}
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

oscmd!(save, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::S(name) = &*va {
			match try_writing_fd(name) {
				Ok(mut fd) => {
					let s = st.to_string();
					match fd.write_all(s.as_bytes()) {
						Ok(()) => { Ok(vec![]) },
						Err(e) => {
							let name = name.to_owned();
							st.mstk.push(va);
							Err(format!("Can't write to state file '{name}': {e}"))
						}
					}
				},
				Err(es) => {
					st.mstk.push(va);
					Err(es)
				}
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

oscmd!(setvar, st {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::S(var), Value::S(val)) => {
					if var.is_empty() {
						st.mstk.push(va);
						st.mstk.push(vb);
						return Err("Empty variable name".into());
					}
					if var.contains(['=', '\0']) {
						st.mstk.push(va);
						st.mstk.push(vb);
						return Err("Variable name must not contain '=' or NUL".into());
					}
					if val.contains('\0') {
						st.mstk.push(va);
						st.mstk.push(vb);
						return Err("Variable value must not contain NUL".into());
					}
					if val.is_empty() { unsafe {
						std::env::remove_var(var);
					}}
					else { unsafe {
						std::env::set_var(var, val);
					}}
					Ok(vec![])
				},
				_ => {
					let ta = TypeLabel::from(&*va);
					let tb = TypeLabel::from(&*vb);
					st.mstk.push(va);
					st.mstk.push(vb);
					Err(format!("Expected two strings, {} and {} given!", ta, tb))
				}
			}
		}
		else {
			st.mstk.push(vb);
			Err("Expected 2 arguments, 1 given!".into())
		}
	}
	else {
		Err("Expected 2 arguments, 0 given!".into())
	}
});

oscmd!(getvar, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::S(var) = &*va {
			if var.is_empty() {
				st.mstk.push(va);
				return Err("Empty variable name".into());
			}
			if var.contains(['=', '\0']) {
				st.mstk.push(va);
				return Err("Variable name must not contain '=' or NUL".into());
			}
			Ok(vec![Value::S(std::env::var_os(var).unwrap_or_default().to_string_lossy().into())])
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

oscmd!(setdir, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::S(path) = &*va {
			match std::env::set_current_dir(path) {
				Ok(()) => {Ok(vec![])},
				Err(e) => {
					let path = path.to_owned();
					st.mstk.push(va);
					Err(format!("Can't set working directory to '{path}': {e}"))
				}
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

oscmd!(getdir, _st {
	match std::env::current_dir() {
		Ok(path) => {Ok(vec![Value::S(path.to_string_lossy().into())])},
		Err(e) => {Err(format!("Can't get current working directory: {e}"))}
	}
});

oscmd!(homedir, _st {
	Ok(vec![Value::S(std::env::home_dir().unwrap_or_default().to_string_lossy().into())])
});

oscmd!(tempdir, _st {
	Ok(vec![Value::S(std::env::temp_dir().to_string_lossy().into())])
});

fn try_child(command: &str, input: &str) -> Result<Child, String> {
	let mut envs = Vec::new();
	let mut cmd = None;
	let mut args = Vec::new();
	for word in command.split(' ') {
		if let Some((var, val)) = word.split_once('=') && cmd.is_none() {	//environment variables
			envs.push((var, val));
		}
		else if cmd.is_none() {	//command
			cmd = Some(word);
		}
		else {	//args
			args.push(word);
		}
	}
	if let Some(cmd) = cmd {
		let mut builder = Command::new(cmd);
		builder.args(args);
		builder.stdin(Stdio::piped());
		builder.stdout(Stdio::piped());
		builder.stderr(Stdio::piped());
		for (var, val) in envs {
			if val.is_empty() {
				builder.env_remove(var);
			}
			else {
				builder.env(var, val);
			}
		}
		match builder.spawn() {
			Ok(mut child) => {
				let mut stdin = child.stdin.take().unwrap();
				stdin.write_all(input.as_bytes()).and_then(|_| stdin.flush()).map_err(|e|
					if let Err(e) = child.kill() { format!("Can't kill child process {}: {e}", child.id()) }
					else { format!("Can't write to input of child process {}: {e}", child.id()) }
				)?;
				Ok(child)
			},
			Err(e) => {
				Err(format!("Can't spawn child process: {e}"))
			}
		}
	}
	else {
		Err("No program name in OS command".into())
	}
}

oscmd!(run, st {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::S(command), Value::S(input)) => {
					match try_child(command, input) {
						Ok(child) => {
							let pid = child.id();
							match child.wait_with_output() {
								Ok(o) => {
									Ok(vec![
										Value::N(o.status.code().unwrap_or_default().into()),
										Value::S(String::from_utf8_lossy(&o.stdout).into()),
										Value::S(String::from_utf8_lossy(&o.stderr).into())
									])
								},
								Err(e) => {
									st.mstk.push(va);
									st.mstk.push(vb);
									Err(format!("Can't wait for child process {pid}: {e}"))
								}
							}
						},
						Err(es) => {
							st.mstk.push(va);
							st.mstk.push(vb);
							Err(es)
						}
					}
				},
				_ => {
					let ta = TypeLabel::from(&*va);
					let tb = TypeLabel::from(&*vb);
					st.mstk.push(va);
					st.mstk.push(vb);
					Err(format!("Expected two strings, {} and {} given!", ta, tb))
				}
			}
		}
		else {
			st.mstk.push(vb);
			Err("Expected 2 arguments, 1 given!".into())
		}
	}
	else {
		Err("Expected 2 arguments, 0 given!".into())
	}
});

oscmd!(spawn, st {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::S(command), Value::S(input)) => {
					let child = try_child(command, input).inspect_err(|_| { st.mstk.push(va); st.mstk.push(vb); })?;
					let pid = child.id();
					unsafe { #[expect(static_mut_refs)] PIDS.insert(pid, child); }
					Ok(vec![Value::N(pid.into())])
				},
				_ => {
					let ta = TypeLabel::from(&*va);
					let tb = TypeLabel::from(&*vb);
					st.mstk.push(va);
					st.mstk.push(vb);
					Err(format!("Expected two strings, {} and {} given!", ta, tb))
				}
			}
		}
		else {
			st.mstk.push(vb);
			Err("Expected 2 arguments, 1 given!".into())
		}
	}
	else {
		Err("Expected 2 arguments, 0 given!".into())
	}
});

oscmd!(wait, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(ra) = &*va {
			if let Ok(pid) = u32::try_from(ra) {
				if let Some(child) = unsafe { #[expect(static_mut_refs)] PIDS.remove(&pid) } {
					match child.wait_with_output() {
						Ok(o) => {
							Ok(vec![
								Value::N(o.status.code().unwrap_or_default().into()),
								Value::S(String::from_utf8_lossy(&o.stdout).into()),
								Value::S(String::from_utf8_lossy(&o.stderr).into())
							])
						},
						Err(e) => {
							st.mstk.push(va);
							Err(format!("Can't get output of child process {pid}: {e}"))
						}
					}
				}
				else {
					st.mstk.push(va);
					Err(format!("Process {pid} is nonexistent or not a child"))
				}
			}
			else {
				let sa = va.to_string();
				st.mstk.push(va);
				Err(format!("{sa} is not a possible process ID"))
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

oscmd!(exited, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(ra) = &*va {
			if let Ok(pid) = u32::try_from(ra) {
				if let Some(child) = unsafe { #[expect(static_mut_refs)] PIDS.get_mut(&pid) } {
					let mut bz = BitVec::new();
					match child.try_wait() {
						Ok(Some(_)) => {
							bz.push(true);
							Ok(vec![Value::B(bz)])
						},
						Ok(None) => {
							bz.push(false);
							Ok(vec![Value::B(bz)])
						},
						Err(e) => {
							st.mstk.push(va);
							Err(format!("Can't get status of child process {}: {e}", child.id()))
						}
					}
				}
				else {
					st.mstk.push(va);
					Err(format!("Process {pid} is nonexistent or not a child"))
				}
			}
			else {
				let sa = va.to_string();
				st.mstk.push(va);
				Err(format!("{sa} is not a possible process ID"))
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

oscmd!(kill, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(ra) = &*va {
			if let Ok(pid) = u32::try_from(ra) {
				if let Some(mut child) = unsafe { #[expect(static_mut_refs)] PIDS.remove(&pid) } {
					match child.kill() {
						Ok(()) => {
							match child.wait_with_output() {
								Ok(o) => {
									Ok(vec![
										Value::N(o.status.code().unwrap_or_default().into()),
										Value::S(String::from_utf8_lossy(&o.stdout).into()),
										Value::S(String::from_utf8_lossy(&o.stderr).into())
									])
								},
								Err(e) => {
									st.mstk.push(va);
									Err(format!("Can't get output of killed child process {pid}: {e}"))
								}
							}
						},
						Err(e) => {
							st.mstk.push(va);
							Err(format!("Can't kill child process {}: {e}", child.id()))
						}
					}
				}
				else {
					st.mstk.push(va);
					Err(format!("Process {pid} is nonexistent or not a child"))
				}
			}
			else {
				let sa = va.to_string();
				st.mstk.push(va);
				Err(format!("{sa} is not a possible process ID"))
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});