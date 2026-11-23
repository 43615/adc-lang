//! State storage and its components, with public APIs

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::str::FromStr;
use std::sync::{Arc, Barrier, Mutex, Once, RwLock};
use std::sync::mpsc::{Sender, Receiver, TryRecvError};
use std::thread::JoinHandle;
use bitvec::prelude::*;
use malachite::{Natural, Rational};
use malachite::base::num::random::RandomPrimitiveInts;
use regex::{Regex, RegexBuilder};
use crate::{ExecResult, IOStreams, LogLevel};
use crate::Utf8Iter;
use Value::*;

fn rng_with(bytes: [u8; 32]) -> RandomPrimitiveInts<u64> {
	malachite::base::num::random::random_primitive_ints(malachite::base::random::Seed::from_bytes(bytes))
}

fn rng_from_os() -> RandomPrimitiveInts<u64> {
	let mut bytes = [0u8; 32];
	getrandom::fill(&mut bytes).unwrap();
	rng_with(bytes)
}

#[derive(Debug)]
pub enum Value {
	B(BitVec<u8, Msb0>),
	N(Rational),
	S(String),
	A(Vec<Value>)
}
impl Default for Value {
	fn default() -> Self {
		A(Vec::new())
	}
}

/// Non-recursive heap DFS to avoid overflowing the stack.
impl Drop for Value {
	fn drop(&mut self) {
		use std::mem::take;
		if let A(a) = self {
			let mut q: Vec<Vec<Value>> = vec![take(a)];	//init queue for arrays to be processed
			while let Some(mut a) = q.pop() {
				for v in &mut a {	//traverse current array
					if let A(aa) = v {	//if a nested array is encountered
						q.push(take(aa))	//move it to the queue
					}
				}
				//current array drops here, with no nested contents
			}
		}
		//scalar values drop themselves after this
	}
}

/// Non-recursive heap BFS to avoid overflowing the stack, using a monadic identity.
impl Clone for Value {
	fn clone(&self) -> Self {
		match self {
			//scalar base cases
			B(b) => B(b.clone()),
			N(n) => N(n.clone()),
			S(s) => S(s.clone()),
			//traverse array using heap pseudorecursion, perform (cloning) identity function on every value
			//`exec1` takes over instead of continuing call recursion, `f` is never called on `A`.
			A(_) => unsafe { crate::fns::exec1(|v, _| Ok(v.clone()), self, false).unwrap_unchecked() }	//SAFETY: clone is infallible
		}
	}
}

impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(A(aa), A(ab)) => {	//compare arrays, heap DFS
				if aa.len() != ab.len() {return false;}
				let mut stk: Vec<(std::slice::Iter<Value>, std::slice::Iter<Value>)> = vec![(aa.iter(), ab.iter())];	//"call stack"
				while let Some((ia, ib)) = stk.last_mut() {	//keep operating on top iters
					if let (Some(va), Some(vb)) = (ia.next(), ib.next()) {	//advance them
						match (va, vb) {
							(A(aa), A(ab)) => {	//nested arrays encountered
								if aa.len() != ab.len() {return false;}
								stk.push((aa.iter(), ab.iter()));	//"recursive call"
							},
							(A(_), _) | (_, A(_)) => {return false;},
							(B(ba), B(bb)) => if ba != bb {return false;},
							(S(sa), S(sb)) => if sa != sb {return false;},
							(N(na), N(nb)) => if na != nb {return false;},
							_ => {return false;}
						}
					}
					else {	//top iters have ended
						stk.pop();	//"return"
					}
				}
				true
			},
			(A(_), _) | (_, A(_)) => false,	//array and scalar
			//both scalars:
			(B(ba), B(bb)) => ba == bb,
			(S(sa), S(sb)) => sa == sb,
			(N(na), N(nb)) => na == nb,
			_ => false
		}
	}
}
impl Eq for Value {}

impl Value {
	pub(crate) fn bb(bit: bool) -> Self {
		let mut v = BitVec::new();
		v.push(bit);
		B(v)
	}

	/// Prints scalar values, passing `A` is undefined behavior. `sb` prints brackets around strings.
	unsafe fn display_scalar(&self, k: usize, o: &Natural, nm: NumOutMode, sb: bool) -> String {
		match self {
			B(b) => {
				b.iter().by_vals().map(|b| if b {'T'} else {'F'}).collect()
			},
			N(n) => {
				use crate::num::*;
				use NumOutMode::*;
				match nm {
					Auto => {
						nauto(n, k, o)
					},
					Norm => {
						let (neg, ipart, fpart, rpart) = digits(n, k, o);
						nnorm(neg, &ipart, &fpart, &rpart, o)
					},
					Sci => {
						let (neg, ipart, fpart, rpart) = digits(n, k, o);
						nsci(neg, &ipart, &fpart, &rpart, o)
					},
					Frac => {
						nfrac(n, k, o)
					},
				}
			},
			S(s) => {
				if sb {
					String::from("[") + s + "]"
				}
				else {
					s.to_owned()
				}
			},
			A(_) => {
				unsafe { std::hint::unreachable_unchecked() }
			}
		}
	}
	
	/// Prints the value, heap DFS to avoid overflowing the stack. `sb` prints brackets around strings.
	pub fn display(&self, k: usize, o: &Natural, nm: NumOutMode, sb: bool) -> String {
		match self {
			A(a) => {	//the fun part
				let mut stk: Vec<std::slice::Iter<Value>> = vec![a.iter()];
				let mut res = String::from('(');
				while let Some(i) = stk.last_mut() {	//keep operating on the top iter
					if let Some(val) = i.next() {	//advance it
						match val {
							A(aa) => {	//nested array encountered
								if res.ends_with(' ') {res.pop();}
								res.push('(');
								stk.push(aa.iter());	//"recursive call"
							},
							_ => {	//scalar encountered
								let s = unsafe { val.display_scalar(k, o, nm, sb) };
								res += &s;	//append it
								res.push(' ');
							}
						}
					}
					else {	//if top iter has ended
						if res.ends_with(' ') {
							res.pop();	//remove trailing space
						}
						res.push(')');
						stk.pop();	//"return"
					}
				}
				res
			},
			_ => {	//just display scalar
				unsafe { self.display_scalar(k, o, nm, sb) }
			}
		}
	}
}

/// Canonical printing function, with default parameters for `N` and brackets for `S`.
impl Display for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.display(DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2, DEFAULT_PARAMS.3, true))
	}
}

/// Tries to convert register index ([`Rational`]) to a string if it was likely set from one, or displays the number with automatic format
pub fn reg_index_pretty(ri: &Rational) -> String {
	use malachite::base::num::conversion::traits::PowerOf2Digits;
	Natural::try_from(ri).ok().and_then(|n| {	//if ri is a natural
		let bytes: Vec<u8> = n.to_power_of_2_digits_desc(8);	//look at its bytes
		str::from_utf8(&bytes).ok().map(|s| String::from("[") + s + "]")	//if it parses to a string, ri was likely set from one
	})
	.unwrap_or_else(|| {
		crate::num::nauto(ri, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2)	//or just return the number in canonical notation
	})
}

/// Output of an ADC thread: Main stack of thread's state + interpreter result
pub type ThreadResult = (Vec<Arc<Value>>, std::io::Result<ExecResult>);

/// Rich handle for ADC threads, usually created and placed in a register by the interpreter.
///
/// Extends [Rust's std threads](std::thread) with a kill signal and appropriate propagation of joins/kills to grandchildren. As such, dropping a handle by means other than [`Self::join`] will make the thread inaccessible.
#[derive(Debug)]
pub struct ThreadHandle {
	fin: Arc<Once>,
	ktx: Sender<()>,
	jbar: Arc<Barrier>,
	jh: JoinHandle<ThreadResult>
}
impl ThreadHandle {
	/// Spawns a new interpreter thread, used by `X` command. Thread has an unbounded lifetime and may only be controlled using the returned handle.
	///
	/// Arguments are similar to the [main interpreter](crate::interpreter), but taken by value since the thread must own them.
	///
	/// `name` sets the thread's name, which is visible at the OS level [where supported](std::thread#naming-threads) and shown in ADC error messages.
	/// Within ADC, the `X` command sets this to a [canonical pretty-print](reg_index_pretty) of the handle register's index.
	/// Any NUL bytes in `name` are removed to avoid OS errors.
	pub fn spawn(name: &str, mut st: State, start: String, io: Arc<Mutex<dyn IOStreams + Send>>, ll: LogLevel, restrict: bool) -> std::io::Result<Self> {
		let (ktx, krx) = std::sync::mpsc::channel::<()>();
		let fin = Arc::new(Once::new());
		let th_fin = Arc::clone(&fin);
		let jbar = Arc::new(Barrier::new(2));
		let th_jbar = Arc::clone(&jbar);
		let tb = std::thread::Builder::new().name(name.replace('\0', ""));
		tb.spawn(move || {
			////////////////////////////////
			let th_res = unsafe { crate::interpreter(&mut st, start.into(), io, ll, Some(&krx), restrict) };
			th_fin.call_once(||());	//latch finished state

			th_jbar.wait();	//wait for join intent from parent
			st.regs.end_threads(	//propagate to children
			th_res.is_err()	//also kill on io error
				||
				match krx.try_recv() {	//check if kill signal arrived after finishing
					Ok(()) => { true },
					Err(TryRecvError::Empty) => { false },
					Err(TryRecvError::Disconnected) => { panic!("parent thread disconnected") }	//only if it's panicking
				}
			);

			(st.mstk, th_res)
			////////////////////////////////
		})
		.map(|jh| Self{fin, ktx, jbar, jh})
	}

	/// Polls thread interpreter status, used by `J` command.
	pub fn is_finished(&self) -> bool {
		self.fin.is_completed()
	}

	/// Terminates thread interpreter ASAP, propagates panic if disconnected. Kill signal is also received after interpreter is finished, for propagation upon [`Self::join`].
	pub fn kill(&self) {
		self.ktx.send(()).unwrap_or_else(|_| panic!("child thread panicked"));
	}

	/// Blocks waiting for the thread to finish and returns its result, or propagates thread's panic. Killing and joining is recursively propagated to child threads.
	pub fn join(self) -> ThreadResult {
		self.jbar.wait();
		self.jh.join().unwrap_or_else(|e| std::panic::resume_unwind(e))
	}
}

#[derive(Default, Debug)]
pub struct Register {
	/// Stack of values, [`Arc`] to allow shallow copies
	pub v: Vec<Arc<Value>>,
	/// Optional thread handle
	pub th: Option<ThreadHandle>
}
impl Register {
	/// Joins the thread handle and saves its resulting values, `kill = true` first sends a kill signal.
	///
	/// Returns a message from any unhappy termination, same as the `j` command.
	pub fn end_thread(&mut self, kill: bool) -> Option<String> {
		use crate::ExecResult::*;

		let mut res = None;

		if let Some(th) = self.th.take() {
			if kill { th.kill(); }
			let (mut vals, th_res) = th.join();
			match th_res {
				Err(e) => {
					res = Some(format!("IO error in thread: {}", e));
				},
				Ok(SoftQuit(c)) if c != 0 => {
					res = Some(format!("Thread quit with code {}", c));
				},
				Ok(HardQuit(c)) if c != 0 => {
					res = Some(format!("Thread hard-quit with code {}", c));
				},
				Ok(Killed) => {
					res = Some("Thread was killed".into());
				},
				_ => {}	//finished or quit with 0
			}

			self.v.append(&mut vals);
		}
		res
	}
}
/// Clone without thread handles since those are unique
impl Clone for Register {
	fn clone(&self) -> Self {
		Self {
			v: self.v.clone(),
			th: None
		}
	}
}
/// Runs `self.end_thread(true)`, prints error message to stderr.
impl Drop for Register {
	fn drop(&mut self) {
		if let Some(s) = self.end_thread(true) {
			eprintln!("{s}");
		}
	}
}

/// Register storage with automatic hybrid indexing
#[derive(Clone, Debug)]
pub struct RegStore {
	/// preallocated range for frequently-accessed registers with ASCII indices (commands like `sa`)
	pub low: [Register; 128],

	/// arbitrary rational indices otherwise
	pub high: HashMap<Rational, Register>
}
impl RegStore {
	/// Get register reference, don't create a register if not present
	pub fn try_get(&self, index: &Rational) -> Option<&Register> {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get(u))
			.or_else(|| self.high.get(index))
	}

	/// Get mutable register reference, don't create a register if not present
	pub fn try_get_mut(&mut self, index: &Rational) -> Option<&mut Register> {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get_mut(u))
			.or_else(|| self.high.get_mut(index))
	}

	/// Get mutable register reference, create register if necessary
	pub fn get_mut(&mut self, index: &Rational) -> &mut Register {
		usize::try_from(index).ok()
			.and_then(|u| self.low.get_mut(u))
			.unwrap_or_else(|| self.high.entry(index.clone()).or_default())
	}

	/// Discard unused registers, reallocate used ones to fit
	pub fn trim(&mut self) {
		for reg in &mut self.low {
			reg.v.shrink_to_fit();
		}
		self.high.retain(|_, reg| {
			reg.v.shrink_to_fit();
			!reg.v.is_empty() || reg.th.is_some()
		});
		self.high.shrink_to_fit();
	}

	/// Clear all values, don't touch thread handles
	pub fn clear_vals(&mut self) {
		for reg in &mut self.low {
			reg.v = Vec::new();
		}
		self.high.retain(|_, reg| {
			reg.v = Vec::new();
			reg.th.is_some()
		});
		self.high.shrink_to_fit();
	}

	/// Replace values with those from `other`, conflicting thread handles in `other` will be killed
	pub fn replace_vals(&mut self, mut other: Self) {
		for (reg, oreg) in self.low.iter_mut().zip(other.low.iter_mut()) {
			std::mem::swap(&mut reg.v, &mut oreg.v);	//swap to reuse allocations, other will be dropped anyway
			if reg.th.is_none() {
				reg.th = oreg.th.take();
			}
		}
		self.high.retain(|ri, reg| {
			if let Some(mut oreg) = other.high.remove(ri) {	//if other has the same reg, overwrite self with its values
				std::mem::swap(&mut reg.v, &mut oreg.v);
				if reg.th.is_none() {
					reg.th = oreg.th.take();
				}
				!reg.v.is_empty() || reg.th.is_some()
				//oreg.th drops here, which kills the thread
			}
			else {	//erase values in self, but keep thread handles
				reg.v = Vec::new();
				reg.th.is_some()
			}
		});
		for (ori, oreg) in other.high.drain().filter(|(_, oreg)| !oreg.v.is_empty()) {	//add remaining regs from other
			self.high.insert(ori, oreg);
		}
	}

	/// Indices of all registers with outstanding threads
	pub fn list_threads(&self) -> Vec<Rational> {
		#[allow(clippy::filter_map_bool_then)]	//not cleaner for tuple lambdas
		self.low.iter().enumerate().filter_map(|(ri, reg)| reg.th.is_some().then(|| Rational::from(ri)))
			.chain(self.high.iter().filter_map(|(ri, reg)| reg.th.is_some().then(|| ri.clone())))
			.collect()
	}

	/// Joins all thread handles and saves their resulting values, `kill = true` first sends kill signals.
	///
	/// Returns messages from any unhappy terminations, same as the `j` command.
	pub fn end_threads(&mut self, kill: bool) -> Vec<String> {
		use crate::ExecResult::*;

		let mut res = Vec::new();
		
		for (ri_nice, reg) in
			self.low.iter_mut().enumerate().map(|(ri, reg)| (reg_index_pretty(&Rational::from(ri)), reg))
				.chain(self.high.iter_mut().map(|(ri, reg)| (reg_index_pretty(ri), reg)))
		{
			if let Some(th) = reg.th.take() {
				if kill {th.kill();}
				let (mut vals, th_res) = th.join();
				match th_res {
					Err(e) => {
						res.push(format!("IO error in thread {}: {}", ri_nice, e));
					},
					Ok(SoftQuit(c)) if c != 0 => {
						res.push(format!("Thread {} quit with code {}", ri_nice, c));
					},
					Ok(HardQuit(c)) if c != 0 => {
						res.push(format!("Thread {} hard-quit with code {}", ri_nice, c));
					},
					Ok(Killed) => {
						res.push(format!("Thread {} was killed", ri_nice));
					},
					_ => {}	//finished or quit with 0
				}

				reg.v.append(&mut vals);
			}
		}
		res
	}
}
impl Default for RegStore {
	fn default() -> Self {
		Self {
			low: std::array::from_fn(|_| Register::default()),
			high: HashMap::default()
		}
	}
}
/// Runs `self.end_threads(true)`, prints error messages to stderr.
impl Drop for RegStore {
	fn drop(&mut self) {
		for s in self.end_threads(true) {
			eprintln!("{s}");
		}
	}
}

/// Global cache for compiled regex automata
#[derive(Default, Debug)]
#[repr(transparent)] pub(crate) struct RegexCache(pub(crate) RwLock<HashMap<String, Regex>>);

impl RegexCache {
	/// Get regex from cache or freshly compile
	pub(crate) fn get(&self, s: &String) -> Result<Regex, String> {
		if let Some(re) = self.0.read().unwrap().get(s) {
			Ok(re.clone())
		}
		else {
			match RegexBuilder::new(s)
				.size_limit(usize::MAX)
				.dfa_size_limit(usize::MAX)
				.build() {
				Ok(re) => {
					self.0.write().unwrap().insert(s.clone(), re.clone());
					Ok(re)
				},
				Err(e) => Err(format!("Can't compile regex: {e}"))
			}
		}
	}

	pub(crate) fn clear(&self) {
		*self.0.write().unwrap() = HashMap::new();
	}
}

/// Number output mode
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)] pub enum NumOutMode { #[default] Auto=0, Norm=1, Sci=2, Frac=3 }

/// Parameter context tuple: (K, I, O, M)
pub type Params = (usize, Natural, Natural, NumOutMode);

/// Default values (0, 10, 10, auto)
pub const DEFAULT_PARAMS: Params = {
	(0, Natural::const_from(10), Natural::const_from(10), NumOutMode::Auto)
};

/// Stack for numeric IO parameter contexts, used implicitly by the ADC interpreter
#[derive(Clone, Debug)]
#[repr(transparent)] pub struct ParamStk(Vec<Params>);
impl ParamStk {
	/// Creates new context with [`DEFAULT_PARAMS`]
	pub fn create(&mut self) {
		self.0.push(DEFAULT_PARAMS)
	}

	/// Returns to previous context, creates default if none left
	pub fn destroy(&mut self) {
		self.0.pop();
		if self.0.is_empty() {self.create();}
	}

	/// Discards all contexts, creates default
	pub fn clear(&mut self) {
		*self = Self::default();
	}

	/// Refers to the underlying [`Vec`] for manual reading
	pub const fn inner(&self) -> &Vec<Params> {
		&self.0
	}

	/// Refers to the underlying [`Vec`] for manual writing
	///
	/// # Safety
	/// Making the [`Vec`] empty will cause *Undefined Behavior* in most of the other methods, which are used implicitly by the interpreter.
	/// Use [`Self::clear`] to create a default context.
	pub const unsafe fn inner_mut(&mut self) -> &mut Vec<Params> {
		&mut self.0
	}

	/// Checked edit of current output precision
	pub fn try_set_k(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(u) = r.try_into() {
			unsafe { self.0.last_mut().unwrap_unchecked().0 = u; }
			Ok(())
		}
		else {Err(concat!("Output precision must be a natural number <= ", cfg_select! {	//usize::MAX exists but nooo this has to be a *literal*
			target_pointer_width = "16" => {"65535"}
			target_pointer_width = "32" => {"4294967295"}
			target_pointer_width = "64" => {"18446744073709551615"}
			_ => compile_error!("Exotic pointer width, please report an issue!")
		}))}
	}

	/// Checked edit of current input base
	pub fn try_set_i(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=1u8 {
			unsafe { self.0.last_mut().unwrap_unchecked().1 = n; }
			Ok(())
		}
		else {Err("Input base must be a natural number >= 2, or 1 for factoradic")}
	}

	/// Checked edit of current output base
	pub fn try_set_o(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(n) = r.try_into() && n>=1u8 {
			unsafe { self.0.last_mut().unwrap_unchecked().2 = n; }
			Ok(())
		}
		else {Err("Output base must be a natural number >= 2, or 1 for factoradic")}
	}

	/// Checked edit of current output mode
	pub fn try_set_m(&mut self, r: &Rational) -> Result<(), &'static str> {
		if let Ok(u) = u8::try_from(r) && u<=3 {
			unsafe { self.0.last_mut().unwrap_unchecked().3 = std::mem::transmute::<u8, NumOutMode>(u); }
			Ok(())
		}
		else {Err("Output mode must be 0|1|2|3")}
	}
	
	/// Infallible edit of current output precision
	pub fn set_k(&mut self, u: usize) {
		unsafe { self.0.last_mut().unwrap_unchecked().0 = u; }
	}

	/// Infallible edit of current output mode
	pub fn set_m(&mut self, m: NumOutMode) {
		unsafe { self.0.last_mut().unwrap_unchecked().3 = m; }
	}

	/// Current output precision
	pub fn get_k(&self) -> usize {
		unsafe { self.0.last().unwrap_unchecked().0 }
	}

	/// Current input base
	pub fn get_i(&self) -> &Natural {
		unsafe { &self.0.last().unwrap_unchecked().1 }
	}

	/// Current output base
	pub fn get_o(&self) -> &Natural {
		unsafe { &self.0.last().unwrap_unchecked().2 }
	}

	/// Current number output mode
	pub fn get_m(&self) -> NumOutMode {
		unsafe { self.0.last().unwrap_unchecked().3 }
	}
}
/// One entry of [`DEFAULT_PARAMS`]
impl Default for ParamStk {
	fn default() -> Self {
		let mut p = Self(Vec::new());
		p.create();
		p
	}
}
/// Length must be at least 1
impl TryFrom<Vec<Params>> for ParamStk {
	type Error = ();

	fn try_from(value: Vec<Params>) -> Result<Self, Self::Error> {
		(!value.is_empty()).then_some(Self(value)).ok_or(())
	}
}

impl From<ParamStk> for Vec<Params> {
	fn from(value: ParamStk) -> Self {
		value.0
	}
}

/// Interpreter state storage bundle
///
/// Typically, just use the [`Default`]. Fields are public to allow for presets and extraction of values, see their documentation.
///
/// `regs` may contain thread handles, which are not copied with [`Clone`]. To preserve thread handles in `self`, use:
/// - `state.clear_vals();` instead of `state = State::default();`
/// - `state.replace_vals(other);` instead of `state = other;`
///
/// The `interpreter…` methods are just an alternative syntax for the functions in the [crate root](crate#functions).
///
/// The [`Display`] implementation produces a custom format that can be executed as an ADC script in order to restore it. This can be done manually or using [`FromStr`].
#[derive(Default, Debug, Clone)]
pub struct State {
	/// Main stack, [`Arc`] to allow shallow copies
	pub mstk: Vec<Arc<Value>>,

	/// Registers
	pub regs: RegStore,

	/// Parameters
	pub params: ParamStk,

	/// RNG state
	pub rng: Option<RandomPrimitiveInts<u64>>
}

/// Serializes it in a standard format, typically for saving as a state file. RNG state is not included.
impl Display for State {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", String::from_utf8_lossy(&Self::STATE_FILE_HEADER))?;
		for (lri, lreg) in self.regs.low.iter().enumerate().filter(|(_, lreg)| !lreg.v.is_empty()) {
			for val in &lreg.v {
				writeln!(f, "{val}")?;
			}
			writeln!(f, "{lri}:ff")?;
		}
		for (hri, hreg) in self.regs.high.iter().filter(|(_, hreg)| !hreg.v.is_empty()) {
			for val in &hreg.v {
				writeln!(f, "{val}")?;
			}
			writeln!(f, "{}:ff", crate::num::nauto(hri, DEFAULT_PARAMS.0, &DEFAULT_PARAMS.2))?;
		}
		for val in &self.mstk {
			writeln!(f, "{val}")?;
		}
		write!(f, "{}", {
			let v: Vec<String> = self.params.inner().iter().map(|par| {
				let mut ps = String::new();
				if par.0 != DEFAULT_PARAMS.0 {ps += &format!("{}k", par.0);}
				if par.2 != DEFAULT_PARAMS.2 {ps += &format!("{}o", par.2);}
				if par.3 != DEFAULT_PARAMS.3 {ps += &format!("{}m", par.3 as u8);}
				if par.1 != DEFAULT_PARAMS.1 {ps += &format!("{}i", par.1);}	//input base last to not affect the others
				ps
			}).collect();
			v.join("{")
		})?;
		Ok(())
	}
}
/// Attempts to deserialize from a previous [`Display`] output, with some basic format checks. Do not use with untrusted input, as creating a malicious script is trivial.
impl FromStr for State {
	type Err = ();

	fn from_str(value: &str) -> Result<Self, Self::Err> {
		if !value.as_bytes().starts_with(&Self::STATE_FILE_HEADER) {return Err(());}
		let start = Utf8Iter::from(value);
		let mut st = Self::default();
		let res = st.interpreter_simple(start, None);
		if !matches!(res, Ok(ExecResult::Finished)) {return Err(());}
		Ok(st)
	}
}
impl State {
	/// Added at the start of saved state files
	pub const STATE_FILE_HEADER: [u8; 20] = *b"# ADC state file v1\n";

	/// Reallocates all fields to fit, does not discard any stored data.
	pub fn trim(&mut self) {
		self.mstk.shrink_to_fit();
		self.regs.trim();
		self.params.0.shrink_to_fit();
	}

	/// Clears all stored data, does not touch thread handles.
	pub fn clear_vals(&mut self) {
		self.mstk = Vec::new();
		self.regs.clear_vals();
		self.params = ParamStk::default();
	}

	/// Replaces stored data with contents of `other`, conflicting thread handles in `other` will be killed.
	pub fn replace_vals(&mut self, other: Self) {
		self.mstk = other.mstk;
		self.regs.replace_vals(other.regs);
		self.params = other.params;
	}

	/// Finds any equal values and replaces them with shallow copies of one of them. Expensive O(n²) operation, use sparingly.
	pub fn dedup(&mut self) {
		let mut regs: Vec<&mut [Arc<Value>]> = vec![&mut self.mstk];	//queue up main stack
		regs.extend(self.regs.low.iter_mut().chain(self.regs.high.values_mut())
			.filter_map(|reg| (!reg.v.is_empty()).then(|| &mut reg.v[..])));	//and all non-empty regs
		while let Some(reg) = regs.pop() {
			for i in 1..=reg.len() {	//goofy manual iteration to avoid redundant comparisons
				let (done, rest) = unsafe { reg.split_at_mut_unchecked(i) };	//isolate unprocessed values
				let sample = unsafe { done.last().unwrap_unchecked() };	//current value
				for other in rest.iter_mut().chain(regs.iter_mut().flat_map(|reg| reg.iter_mut())) {    //look at rest and all other regs
					if !Arc::ptr_eq(sample, other) && (**sample == **other) { *other = Arc::clone(sample) }    //dedup if different pointers but equal values
				}
			}
		}
	}

	/// Seed RNG with a preset value
	pub fn seed_rng_with(&mut self, bytes: [u8; 32]) {
		self.rng = Some(rng_with(bytes));
	}

	/// Seed RNG from [an OS source](getrandom#supported-targets)
	pub fn seed_rng_from_os(&mut self) {
		self.rng = Some(rng_from_os());
	}
	
	/// Clear RNG state, the next `N` command will seed it from the OS
	pub const fn reset_rng(&mut self) {
		self.rng = None;
	}

	/// Access RNG, or seed it with a preset value
	pub fn get_rng_or_seed_with(&mut self, bytes: [u8; 32]) -> &mut RandomPrimitiveInts<u64> {
		self.rng.get_or_insert_with(|| rng_with(bytes))
	}

	/// Access RNG, or seed it from the OS
	pub fn get_rng_or_seed_from_os(&mut self) -> &mut RandomPrimitiveInts<u64> {
		self.rng.get_or_insert_with(rng_from_os)
	}
	
	#[expect(clippy::missing_safety_doc)]
	pub unsafe fn interpreter(
		&mut self,
		start: Utf8Iter,
		io: Arc<Mutex<dyn IOStreams + Send>>,
		ll: LogLevel,
		kill: Option<&Receiver<()>>,
		restrict: bool
	) -> std::io::Result<ExecResult>
	{
		unsafe { crate::interpreter(self, start, io, ll, kill, restrict) }
	}

	#[expect(clippy::missing_safety_doc)]
	pub unsafe fn interpreter_no_io(
		&mut self,
		start: Utf8Iter,
		kill: Option<&Receiver<()>>,
		restrict: bool
	) -> std::io::Result<ExecResult>
	{
		unsafe { crate::interpreter_no_io(self, start, kill, restrict) }
	}

	pub fn interpreter_no_os(
		&mut self,
		start: Utf8Iter,
		io: Arc<Mutex<dyn IOStreams + Send>>,
		ll: LogLevel,
		kill: Option<&Receiver<()>>,
	) -> std::io::Result<ExecResult>
	{
		crate::interpreter_no_os(self, start, io, ll, kill)
	}

	pub fn interpreter_simple(
		&mut self,
		start: Utf8Iter,
		kill: Option<&Receiver<()>>
	) -> std::io::Result<ExecResult>
	{
		crate::interpreter_simple(self, start, kill)
	}
}