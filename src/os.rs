//! OS-interfacing commands, restricted to single-threaded access

use std::cell::LazyCell;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::process::{Command, Stdio, Child};
use std::sync::Mutex;
use bitvec::prelude::*;
use crate::err::TypeLabel;
use crate::state::{Value::{self, *}, State};

/// OS command definition
pub type OsCmd = fn(&mut State) -> Result<Vec<Value>, String>;
/// Function template for OS command
macro_rules! oscmd {
    ($name:ident, $st:ident $block:block) => {
		fn $name($st: &mut State) -> Result<Vec<Value>, String> {
			$block
		}
	}
}

/// Map of function names. The contained [OsCmd]s may only be used while a lock is held.
pub static OS_CMDS: Mutex<phf::Map<&[u8], OsCmd>> = Mutex::new(phf::phf_map! {
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
});

/// Persistent map of child process handles. Multithreaded access is impossible due to the need to lock OS_CMDS.
static mut PIDS: LazyCell<HashMap<u32, Child>> = LazyCell::new(HashMap::new);

oscmd!(osarch, _st {
	Ok(vec![S(std::env::consts::ARCH.into())])
});

oscmd!(osfamily, _st {
	Ok(vec![S(std::env::consts::FAMILY.into())])
});

oscmd!(osname, _st {
	Ok(vec![S(std::env::consts::OS.into())])
});

oscmd!(pid, _st {
	Ok(vec![N(std::process::id().into())])
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

fn read<const B: bool>(st: &mut State) -> Result<Vec<Value>, String> {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let S(name) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected string, {} given!", ta));
	};
	let mut v = Vec::new();
	match try_reading_fd(name) {
		Ok(mut fd) => {
			match fd.read_to_end(&mut v) {
				Ok(_) => {
					if B {
						Ok(vec![A(
							v.into_iter().map(|b| N(b.into())).collect()
						)])
					}
					else {
						Ok(vec![S(
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

fn append<const B: bool>(st: &mut State) -> Result<Vec<Value>, String> {
	let Some(vb) = st.mstk.pop() else {return Err("Expected 2 arguments, 0 given!".into());};
	let Some(va) = st.mstk.pop() else {
		st.mstk.push(vb);
		return Err("Expected 2 arguments, 1 given!".into());
	};
	match (&*va, &*vb) {
		(A(a), S(name)) if B => {
			match try_appending_fd(name) {
				Ok(mut fd) => {
					let mut bytes = Vec::new();
					for v in a {
						let N(r) = v else {
							let t = TypeLabel::from(v);
							st.mstk.push(va);
							st.mstk.push(vb);
							return Err(format!("Expected byte array, found {} in array!", t));
						};
						let Ok(u) = u8::try_from(r) else {
							st.mstk.push(va);
							st.mstk.push(vb);
							return Err("Expected byte array, found non-byte number in array!".into());
						};
						bytes.push(u);
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
		(S(s), S(name)) if !B => {
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

fn write<const B: bool>(st: &mut State) -> Result<Vec<Value>, String> {
	let Some(vb) = st.mstk.pop() else {return Err("Expected 2 arguments, 0 given!".into());};
	let Some(va) = st.mstk.pop() else {
		st.mstk.push(vb);
		return Err("Expected 2 arguments, 1 given!".into());
	};
	match (&*va, &*vb) {
		(A(a), S(name)) if B => {
			match try_writing_fd(name) {
				Ok(mut fd) => {
					let mut bytes = Vec::new();
					for v in a {
						let N(r) = v else {
							let t = TypeLabel::from(v);
							st.mstk.push(va);
							st.mstk.push(vb);
							return Err(format!("Expected byte array, found {} in array!", t));
						};
						let Ok(u) = u8::try_from(r) else {
							st.mstk.push(va);
							st.mstk.push(vb);
							return Err("Expected byte array, found non-byte number in array!".into());
						};
						bytes.push(u);
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
		(S(s), S(name)) if !B => {
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

oscmd!(load, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let S(name) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected string, {} given!", ta));
	};
	let mut v = Vec::new();
	match try_reading_fd(name) {
		Ok(mut fd) => {
			match fd.read_to_end(&mut v) {
				Ok(_) => {
					match State::try_from(v.as_slice()) {
						Ok(nst) => {
							st.replace_vals(nst);
							Ok(vec![])
						},
						Err(()) => {
							let name = name.to_owned();
							st.mstk.push(va);
							Err(format!("State file '{name}' is invalid"))
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
});

oscmd!(save, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let S(name) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected string, {} given!", ta));
	};
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
});

oscmd!(setvar, st {
	let Some(vb) = st.mstk.pop() else {return Err("Expected 2 arguments, 0 given!".into());};
	let Some(va) = st.mstk.pop() else {
		st.mstk.push(vb);
		return Err("Expected 2 arguments, 1 given!".into());
	};
	match (&*va, &*vb) {
		(S(var), S(val)) => {
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
});

oscmd!(getvar, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let S(var) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected string, {} given!", ta));
	};
	if var.is_empty() {
		st.mstk.push(va);
		return Err("Empty variable name".into());
	}
	if var.contains(['=', '\0']) {
		st.mstk.push(va);
		return Err("Variable name must not contain '=' or NUL".into());
	}
	Ok(vec![S(std::env::var_os(var).unwrap_or_default().to_string_lossy().into())])
});

oscmd!(setdir, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let S(path) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected string, {} given!", ta));
	};
	match std::env::set_current_dir(path) {
		Ok(()) => {Ok(vec![])},
		Err(e) => {
			let path = path.to_owned();
			st.mstk.push(va);
			Err(format!("Can't set working directory to '{path}': {e}"))
		}
	}
});

oscmd!(getdir, _st {
	match std::env::current_dir() {
		Ok(path) => {Ok(vec![S(path.to_string_lossy().into())])},
		Err(e) => {Err(format!("Can't get current working directory: {e}"))}
	}
});

oscmd!(homedir, _st {
	Ok(vec![S(std::env::home_dir().unwrap_or_default().to_string_lossy().into())])
});

oscmd!(tempdir, _st {
	Ok(vec![S(std::env::temp_dir().to_string_lossy().into())])
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
	let Some(cmd) = cmd else {return Err("No program name in OS command".into());};
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

oscmd!(run, st {
	let Some(vb) = st.mstk.pop() else {return Err("Expected 2 arguments, 0 given!".into());};
	let Some(va) = st.mstk.pop() else {
		st.mstk.push(vb);
		return Err("Expected 2 arguments, 1 given!".into());
	};
	match (&*va, &*vb) {
		(S(input), S(command)) => {
			match try_child(command, input) {
				Ok(child) => {
					let pid = child.id();
					match child.wait_with_output() {
						Ok(o) => {
							Ok(vec![
								N(o.status.code().unwrap_or_default().into()),
								S(String::from_utf8_lossy(&o.stdout).into()),
								S(String::from_utf8_lossy(&o.stderr).into())
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
});

oscmd!(spawn, st {
	let Some(vb) = st.mstk.pop() else {return Err("Expected 2 arguments, 0 given!".into());};
	let Some(va) = st.mstk.pop() else {
		st.mstk.push(vb);
		return Err("Expected 2 arguments, 1 given!".into());
	};
	match (&*va, &*vb) {
		(S(input), S(command)) => {
			let child = try_child(command, input).inspect_err(|_| { st.mstk.push(va); st.mstk.push(vb); })?;
			let pid = child.id();
			unsafe {
				#[expect(static_mut_refs)]
				if PIDS.insert(pid, child).is_some() {
					panic!("Impossible OS error: Duplicate child process ID: {pid}");
				}
			}
			Ok(vec![N(pid.into())])
		},
		_ => {
			let ta = TypeLabel::from(&*va);
			let tb = TypeLabel::from(&*vb);
			st.mstk.push(va);
			st.mstk.push(vb);
			Err(format!("Expected two strings, {} and {} given!", ta, tb))
		}
	}
});

oscmd!(wait, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(ra) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	let Ok(pid) = u32::try_from(ra) else {
		let sa = va.to_string();
		st.mstk.push(va);
		return Err(format!("{sa} is not a possible process ID"));
	};
	let Some(child) = (unsafe{#[expect(static_mut_refs)] PIDS.remove(&pid)}) else {
		st.mstk.push(va);
		return Err(format!("Process {pid} is nonexistent or not a child"));
	};
	match child.wait_with_output() {
		Ok(o) => {
			Ok(vec![
				N(o.status.code().unwrap_or_default().into()),
				S(String::from_utf8_lossy(&o.stdout).into()),
				S(String::from_utf8_lossy(&o.stderr).into())
			])
		},
		Err(e) => {
			st.mstk.push(va);
			Err(format!("Can't get output of child process {pid}: {e}"))
		}
	}
});

oscmd!(exited, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(ra) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	let Ok(pid) = u32::try_from(ra) else {
		let sa = va.to_string();
		st.mstk.push(va);
		return Err(format!("{sa} is not a possible process ID"));
	};
	let Some(mut child) = (unsafe{#[expect(static_mut_refs)] PIDS.remove(&pid)}) else {
		st.mstk.push(va);
		return Err(format!("Process {pid} is nonexistent or not a child"));
	};
	match child.try_wait() {
		Ok(Some(_)) => {
			Ok(vec![B(bitvec![u8, Msb0; 1])])
		},
		Ok(None) => {
			Ok(vec![B(bitvec![u8, Msb0; 0])])
		},
		Err(e) => {
			st.mstk.push(va);
			Err(format!("Can't get status of child process {}: {e}", child.id()))
		}
	}
});

oscmd!(kill, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(ra) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	let Ok(pid) = u32::try_from(ra) else {
		let sa = va.to_string();
		st.mstk.push(va);
		return Err(format!("{sa} is not a possible process ID"));
	};
	let Some(mut child) = (unsafe{#[expect(static_mut_refs)] PIDS.remove(&pid)}) else {
		st.mstk.push(va);
		return Err(format!("Process {pid} is nonexistent or not a child"));
	};
	match child.kill() {
		Ok(()) => {
			match child.wait_with_output() {
				Ok(o) => {
					Ok(vec![
						N(o.status.code().unwrap_or_default().into()),
						S(String::from_utf8_lossy(&o.stdout).into()),
						S(String::from_utf8_lossy(&o.stderr).into())
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
});