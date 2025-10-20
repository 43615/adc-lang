//! OS-interfacing commands

use crate::cmds::*;
use crate::structs::{State, Value};
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use crate::errors::TypeLabel;

pub(crate) const OS_CMDS: phf::Map<&[u8], Cmd> = phf::phf_map! {
	b"osarch" => osarch,
	b"osfamily" => osfamily,
	b"osname" => osname,
	b"read" => read,
	b"append" => append,
	b"write" => write,
};

cmd!(osarch, _st {
	Ok(vec![Value::S(std::env::consts::ARCH.into())])
});

cmd!(osfamily, _st {
	Ok(vec![Value::S(std::env::consts::FAMILY.into())])
});

cmd!(osname, _st {
	Ok(vec![Value::S(std::env::consts::OS.into())])
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

cmd!(read, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::S(name) = &*va {
			let mut v = Vec::new();
			let mut fd = try_reading_fd(name)?;
			fd.read_to_end(&mut v).map_err(|e| format!("Can't read from file '{name}': {e}"))?;
			Ok(vec![Value::S(crate::structs::Utf8Iter::from(v).try_into()?)])
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected string, {} given", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given".into())
	}
});

cmd!(append, st {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::S(s), Value::S(name)) => {
					let mut fd = try_appending_fd(name)?;
					fd.write_all(s.as_bytes()).map_err(|e| format!("Can't append to file '{name}': {e}"))?;
					Ok(vec![])
				},
				_ => {
					let ta = TypeLabel::from(&*va);
					let tb = TypeLabel::from(&*vb);
					st.mstk.push(va);
					st.mstk.push(vb);
					Err(format!("Expected two strings, {} and {} given", ta, tb))
				}
			}
		}
		else {
			Err("Expected 2 arguments, 1 given".into())
		}
	}
	else {
		Err("Expected 2 arguments, 0 given".into())
	}
});

cmd!(write, st {
	if let Some(vb) = st.mstk.pop() {
		if let Some(va) = st.mstk.pop() {
			match (&*va, &*vb) {
				(Value::S(s), Value::S(name)) => {
					let mut fd = try_writing_fd(name)?;
					fd.write_all(s.as_bytes()).map_err(|e| format!("Can't write to file '{name}': {e}"))?;
					Ok(vec![])
				},
				_ => {
					let ta = TypeLabel::from(&*va);
					let tb = TypeLabel::from(&*vb);
					st.mstk.push(va);
					st.mstk.push(vb);
					Err(format!("Expected two strings, {} and {} given", ta, tb))
				}
			}
		}
		else {
			Err("Expected 2 arguments, 1 given".into())
		}
	}
	else {
		Err("Expected 2 arguments, 0 given".into())
	}
});