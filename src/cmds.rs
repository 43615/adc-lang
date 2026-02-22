//! Impure commands that access the whole state, manual type matching and application.
//!
//! New resulting values are written to a `Vec` to enable redirection to array input.
//!
//! Errors ending in `!` are displayed as syntax errors, others as value errors (in-band hack to keep the return type composable).

use crate::errors::TypeLabel;
use crate::structs::{Value, State};

/// Impure command
pub type Cmd = fn(&mut State) -> Result<Vec<Value>, String>;
/// Function template for impure command
macro_rules! cmd {
    ($name:ident, $st:ident $block:block) => {
		pub fn $name($st: &mut State) -> Result<Vec<Value>, String> {
			$block
		}
	}
}

cmd!(sk, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(r) = &*va {
			st.params.try_set_k(r)?;
			Ok(vec![])
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected number, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

cmd!(si, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(r) = &*va {
			st.params.try_set_i(r)?;
			Ok(vec![])
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected number, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

cmd!(so, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(r) = &*va {
			st.params.try_set_o(r)?;
			Ok(vec![])
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected number, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

cmd!(sm, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(r) = &*va {
			st.params.try_set_m(r)?;
			Ok(vec![])
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected number, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

cmd!(gk, st {
	Ok(vec![Value::N(st.params.get_k().into())])
});

cmd!(gi, st {
	Ok(vec![Value::N(st.params.get_i().into())])
});

cmd!(go, st {
	Ok(vec![Value::N(st.params.get_o().into())])
});

cmd!(gm, st {
	Ok(vec![Value::N((st.params.get_m() as u8).into())])
});

cmd!(cbo, st {
	st.params.create();
	Ok(vec![])
});

cmd!(cbc, st {
	st.params.destroy();
	Ok(vec![])
});

cmd!(cls, st {
	st.mstk.clear();
	Ok(vec![])
});

cmd!(cln, st {
	if let Some(va) = st.mstk.pop() {
		if let Value::N(r) = &*va {
			if let Ok(u) = usize::try_from(r) {
				if let Some(len) = st.mstk.len().checked_sub(u) {
					st.mstk.truncate(len);
					Ok(vec![])
				}
				else {
					st.mstk.push(va);
					Err(format!("Can't clear {} values, stack depth is {}", u, st.mstk.len() - 1))
				}
			}
			else {
				let vs = va.to_string();
				st.mstk.push(va);
				Err(format!("Can't possibly clear {} values", vs))
			}
		}
		else {
			let ta = TypeLabel::from(&*va);
			st.mstk.push(va);
			Err(format!("Expected number, {} given!", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given!".into())
	}
});

cmd!(rev, st {
	let len = st.mstk.len();
	if len >= 2 {
		st.mstk.swap(len - 1, len - 2);
	}	//else no-op
	Ok(vec![])
});