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

/// OS command definition
pub type OsCmd = unsafe fn(&mut State) -> Result<Vec<Value>, String>;
/// Function template for OS command, `unsafe` as an internal lint
macro_rules! oscmd {
    ($name:ident, $st:ident $block:block) => {
		pub unsafe fn $name($st: &mut State) -> Result<Vec<Value>, String> {
			$block
		}
	}
}

pub const OS_CMDS: phf::Map<&[u8], OsCmd> = phf::phf_map! {
	//static env queries
	b"osarch" => osarch,
	b"osfamily" => osfamily,
	b"osname" => osname,
	b"pid" => pid,

	//file operations
	b"read" => read::<false>,
	b"append" => append::<false>,
	b"write" => write::<false>,
	b"readb" => read::<true>,
	b"appendb" => append::<true>,
	b"writeb" => write::<true>,
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
/// SAFETY: Multithreaded access is forbidden by interpreter safety contract and the flag below.
static mut PIDS: LazyCell<HashMap<u32, Child>> = LazyCell::new(HashMap::new);

/// `true` during OS command execution, likely failsafe to catch misuse of interpreter function.
pub static mut ACCESS: bool = false;

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

pub unsafe fn read<const B: bool>(st: &mut State) -> Result<Vec<Value>, String> {
	if let Some(va) = st.mstk.pop() {
		if let Value::S(name) = &*va {
			let mut v = Vec::new();
			match try_reading_fd(name) {
				Ok(mut fd) => {
					match fd.read_to_end(&mut v) {
						Ok(_) => {
							if B {
								Ok(vec![Value::A(
									v.into_iter().map(|b| Value::N(b.into())).collect()
								)])
							}
							else {
								Ok(vec![Value::S(
									String::from_utf8_lossy(&v).into())
								])
							}
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
}

pub unsafe fn append<const B: bool>(st: &mut State) -> Result<Vec<Value>, String> {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::A(a), Value::S(name)) if B => {
					match try_appending_fd(name) {
						Ok(mut fd) => {
							let mut bytes = Vec::new();
							for v in a {
								if let Value::N(r) = v {
									if let Ok(u) = u8::try_from(r) {
										bytes.push(u);
									}
									else {
										st.mstk.push(va);
										st.mstk.push(vb);
										return Err("Expected byte array, found non-byte number in array!".into());
									}
								}
								else {
									let t = TypeLabel::from(v);
									st.mstk.push(va);
									st.mstk.push(vb);
									return Err(format!("Expected byte array, found {} in array!", t));
								}
							}
							match fd.write_all(&bytes) {
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
				(Value::S(s), Value::S(name)) if !B => {
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
					Err(
						if B {
							format!("Expected array and string, {} and {} given!", ta, tb)
						}
						else {
							format!("Expected two strings, {} and {} given!", ta, tb)
						}
					)
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
}

pub unsafe fn write<const B: bool>(st: &mut State) -> Result<Vec<Value>, String> {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::A(a), Value::S(name)) if B => {
					match try_writing_fd(name) {
						Ok(mut fd) => {
							let mut bytes = Vec::new();
							for v in a {
								if let Value::N(r) = v {
									if let Ok(u) = u8::try_from(r) {
										bytes.push(u);
									}
									else {
										st.mstk.push(va);
										st.mstk.push(vb);
										return Err("Expected byte array, found non-byte number in array!".into());
									}
								}
								else {
									let t = TypeLabel::from(v);
									st.mstk.push(va);
									st.mstk.push(vb);
									return Err(format!("Expected byte array, found {} in array!", t));
								}
							}
							match fd.write_all(&bytes) {
								Ok(()) => {
									Ok(vec![])
								},
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
				(Value::S(s), Value::S(name)) if !B => {
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
					Err(
						if B {
							format!("Expected array and string, {} and {} given!", ta, tb)
						}
						else {
							format!("Expected two strings, {} and {} given!", ta, tb)
						}
					)
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
}

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
								
								let res = crate::interpreter_simple(&mut new_st, st_mac, None);	//one recursive call as a treat
								
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
				(Value::S(input), Value::S(command)) => {
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
				(Value::S(input), Value::S(command)) => {
					let child = try_child(command, input).inspect_err(|_| { st.mstk.push(va); st.mstk.push(vb); })?;
					let pid = child.id();
					unsafe {
						#[expect(static_mut_refs)]
						if PIDS.insert(pid, child).is_some() {
							panic!("Impossible OS error: Duplicate child process ID: {pid}");
						}
					}
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
					match child.try_wait() {
						Ok(Some(_)) => {
							Ok(vec![Value::B(bitvec![u8, Lsb0; 1])])
						},
						Ok(None) => {
							Ok(vec![Value::B(bitvec![u8, Lsb0; 0])])
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