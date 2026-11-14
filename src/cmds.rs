//! Impure commands that access the whole state, manual type matching and application.
//!
//! New resulting values are written to a `Vec` to enable redirection to array input.
//!
//! Errors ending in `!` are displayed as syntax errors, others as value errors (in-band hack to keep the return type composable).

use crate::err::TypeLabel;
use crate::state::{Value::{self, *}, State};

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

//   k
cmd!(sk, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(r) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	st.params.try_set_k(r)?;
	Ok(vec![])
});

//   i
cmd!(si, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(r) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	st.params.try_set_i(r)?;
	Ok(vec![])
});

//   o
cmd!(so, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(r) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	st.params.try_set_o(r)?;
	Ok(vec![])
});

//   m
cmd!(sm, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(r) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	st.params.try_set_m(r)?;
	Ok(vec![])
});

//   K
cmd!(gk, st {
	Ok(vec![N(st.params.get_k().into())])
});

//   I
cmd!(gi, st {
	Ok(vec![N(st.params.get_i().into())])
});

//   O
cmd!(go, st {
	Ok(vec![N(st.params.get_o().into())])
});

//   M
cmd!(gm, st {
	Ok(vec![N((st.params.get_m() as u8).into())])
});

//   {
cmd!(cbo, st {
	st.params.create();
	Ok(vec![])
});

//   }
cmd!(cbc, st {
	st.params.destroy();
	Ok(vec![])
});

//   c
cmd!(cls, st {
	st.mstk.clear();
	Ok(vec![])
});

//   C
cmd!(cln, st {
	let Some(va) = st.mstk.pop() else {return Err("Expected 1 argument, 0 given!".into());};
	let N(r) = &*va else {
		let ta = TypeLabel::from(&*va);
		st.mstk.push(va);
		return Err(format!("Expected number, {} given!", ta));
	};
	let Ok(u) = usize::try_from(r) else {
		let vs = va.to_string();
		st.mstk.push(va);
		return Err(format!("Can't possibly clear {} values", vs));
	};
	let Some(len) = st.mstk.len().checked_sub(u) else {
		st.mstk.push(va);
		return Err(format!("Can't clear {} values, stack depth is {}", u, st.mstk.len() - 1));
	};
	st.mstk.truncate(len);
	Ok(vec![])
});

//   r
cmd!(rev, st {
	let len = st.mstk.len();
	if len >= 2 {
		st.mstk.swap(len - 1, len - 2);
	}	//else no-op
	Ok(vec![])
});