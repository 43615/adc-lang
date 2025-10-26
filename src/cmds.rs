//! Impure commands that access the whole state, manual application.
//!
//! New resulting values are written to a `Vec` to enable redirection to array input.

use malachite::base::num::basic::traits::Zero;
use crate::structs::{State, Value};
use crate::errors::TypeLabel;
use malachite::Rational;

/// Impure command
pub(crate) type Cmd = fn(&mut State) -> Result<Vec<Value>, String>;
/// Function template for impure command
macro_rules! cmd {
    ($name:ident, $st:ident $block:block) => {
		pub(crate) fn $name($st: &mut State) -> Result<Vec<Value>, String> {
			$block
		}
	}
}
pub(crate) use cmd;

/// Impure command with register access
pub(crate) type CmdR = fn(&mut State, &Rational) -> Result<Vec<Value>, String>;
/// Function template for impure command with register access
macro_rules! cmdr {
    ($name:ident, $st:ident, $ri:ident $block:block) => {
		pub(crate) fn $name($st: &mut State, $ri: &Rational) -> Result<Vec<Value>, String> {
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
			Err(format!("Expected number, {} given", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given".into())
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
			Err(format!("Expected number, {} given", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given".into())
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
			Err(format!("Expected number, {} given", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given".into())
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
			Err(format!("Expected number, {} given", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given".into())
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
				st.mstk.truncate(st.mstk.len().saturating_sub(u));
				Ok(vec![])
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
			Err(format!("Expected number, {} given", ta))
		}
	}
	else {
		Err("Expected 1 argument, 0 given".into())
	}
});

cmd!(rev, st {
	let len = st.mstk.len();
	if len >= 2 {
		st.mstk.swap(len - 1, len - 2);
	}	//else no-op
	Ok(vec![])
});

cmdr!(rz, st, ri {
	if let Some(reg) = st.regs.try_get(ri) {
		Ok(vec![Value::N(reg.v.len().into())])
	}
	else {
		Ok(vec![Value::N(Rational::ZERO)])
	}
});