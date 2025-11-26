//! Pure functions and rank-polymorphic execution engine
//! 
//! Functions have 1, 2, or 3 `&Value` parameters (monadic, dyadic, triadic) and a boolean mode switch (false by default, enabled by \` command).

use std::collections::VecDeque;
use std::ptr::NonNull;
use bitvec::prelude::*;
use malachite::{Natural, Rational};
use malachite::base::num::arithmetic::traits::{Abs, CheckedLogBase, CheckedRoot, CheckedSqrt, DivMod, Factorial, Mod, ModInverse, ModPow, Pow, Reciprocal};
use malachite::base::num::basic::traits::{NegativeOne, Zero, One};
use crate::errors::FnErr::{self, *};
use crate::structs::Value::{self, *};
use crate::conv::*;
use crate::RE_CACHE;

/// Monadic function definition
pub type Mon = fn(&Value, bool) -> Result<Value, FnErr>;
/// Monadic template with standard type matching
macro_rules! mon {
    ($name:ident $($pa:pat, $m:pat => $op:expr),*) => {
		pub fn $name(a: &Value, m: bool) -> Result<Value, FnErr> {
			match (a, m) {
				$(($pa, $m) => $op,)*
				_ => Err(Type1(a.into()))
			}
		}
	}
}

/// Dyadic function definition
pub type Dya = fn(&Value, &Value, bool) -> Result<Value, FnErr>;
/// Dyadic template with standard type matching
macro_rules! dya {
    ($name:ident $($pa:pat, $pb:pat, $m:pat => $op:expr),*) => {
		pub fn $name(a: &Value, b: &Value, m: bool) -> Result<Value, FnErr> {
			match (a, b, m) {
				$(($pa, $pb, $m) => $op,)*
				_ => Err(Type2(a.into(), b.into()))
			}
		}
	}
}

/// Triadic function definition
pub type Tri = fn(&Value, &Value, &Value, bool) -> Result<Value, FnErr>;
/// Triadic template with standard type matching
macro_rules! tri {
    ($name:ident $($pa:pat, $pb:pat, $pc:pat, $m:pat => $op:expr),*) => {
		pub fn $name(a: &Value, b: &Value, c: &Value, m: bool) -> Result<Value, FnErr> {
			match (a, b, c, m) {
				$(($pa, $pb, $pc, $m) => $op,)*
				_ => Err(Type3(a.into(), b.into(), c.into()))
			}
		}
	}
}

/// execute monadic fn, pseudorecursion through nested arrays
pub fn exec1(f: Mon, a: &Value, m: bool) -> Result<Value, FnErr> {
	if let A(aa) = a { unsafe {	//iterate through array, bfs without recursion
		//NonNull pointers are required because of aliasing rules, soundness is ensured manually
		let mut az: Vec<Value> = Vec::new();	//resulting array
		let mut q: VecDeque<(&Vec<Value>, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays
		q.push_back((aa, (&mut az).into()));
		while let Some((src, mut dst)) = q.pop_front() {	//keep reading from front of queue
			for val in src {	//iterate through current layer
				if let A(nsrc) = val {	//array encountered, pseudorecursion needed
					dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
					let A(ndst) = dst.as_mut().last_mut().unwrap_unchecked() else { std::hint::unreachable_unchecked() };	//get pointer to destination
					q.push_back((nsrc, ndst.into()));	//add nested source and destination arrays to queue
				}
				else {	//scalar value, compute and store result
					dst.as_mut().push(f(val, m)?);
				}
			}
		}
		Ok(A(az))
	}}
	else {	//just call function
		f(a, m)
	}
}

/// execute dyadic fn, pseudorecursion through nested arrays
pub fn exec2(f: Dya, a: &Value, b: &Value, m: bool) -> Result<Value, FnErr> {
	match (a, b) {
		(A(_), A(_)) | (A(_), _) | (_, A(_)) => { unsafe {	//iterate through array(s), bfs without recursion
			//NonNull pointers are required because of aliasing rules, soundness is ensured manually
			let mut az: Vec<Value> = Vec::new();	//resulting array
			let mut q: VecDeque<(&Value, &Value, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays, scalar values in 0,1 get promoted later
			q.push_back((a, b, (&mut az).into()));
			while let Some((a, b, mut dst)) = q.pop_front() {	//keep reading from front of queue
				for (va, vb) in {lenck2(a, b)?; PromotingIter::from(a).zip(PromotingIter::from(b))} {
					match (va, vb) {
						(A(_), A(_)) | (A(_), _) | (_, A(_)) => {	//one or both elements are arrays
							dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
							let A(ndst) = dst.as_mut().last_mut().unwrap_unchecked() else { std::hint::unreachable_unchecked() };	//get pointer to destination
							q.push_back((va, vb, ndst.into()));	//add nested source and destination arrays to queue
						}
						(_, _) => {	//scalar value, compute and store result
							dst.as_mut().push(f(va, vb, m)?);
						}
					}
				}
			}
			Ok(A(az))
		}}
		(_, _) => {	//just call function
			f(a, b, m)
		}
	}
}

/// execute triadic fn, pseudorecursion through nested arrays
pub fn exec3(f: Tri, a: &Value, b: &Value, c: &Value, m: bool) -> Result<Value, FnErr> {
	match (a, b, c) {
		(A(_), A(_), A(_)) |
		(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
		(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => { unsafe {	//iterate through array(s), bfs without recursion
			//NonNull pointers are required because of aliasing rules, soundness is ensured manually
			let mut az: Vec<Value> = Vec::new();	//resulting array
			let mut q: VecDeque<(&Value, &Value, &Value, NonNull<Vec<Value>>)> = VecDeque::new();	//queue of source/destination arrays, scalar values in 0,1,2 get promoted later
			q.push_back((a, b, c, (&mut az).into()));
			while let Some((a, b, c, mut dst)) = q.pop_front() {    //keep reading from front of queue
				for ((va, vb), vc) in {lenck3(a, b, c)?; PromotingIter::from(a).zip(PromotingIter::from(b)).zip(PromotingIter::from(c))} {
					match (va, vb, vc) {
						(A(_), A(_), A(_)) |
						(A(_), A(_), _) | (A(_), _, A(_)) | (_, A(_), A(_)) |
						(A(_), _, _) | (_, A(_), _) | (_, _, A(_)) => {	//one, two, or three elements are arrays
							dst.as_mut().push(A(Vec::new()));	//allocate destination, mirroring the source's array nesting
							let A(ndst) = dst.as_mut().last_mut().unwrap_unchecked() else { std::hint::unreachable_unchecked() };	//get pointer to destination
							q.push_back((va, vb, vc, ndst.into()));	//add nested source and destination arrays to queue
						}
						(_, _, _) => {	//scalar value, compute and store result
							dst.as_mut().push(f(va, vb, vc, m)?);
						}
					}
				}
			}
			Ok(A(az))
		}}
		(_, _, _) => {	//just call function
			f(a, b, c, m)
		}
	}
}

dya!(add
	B(ba), B(bb), _ => {	//concat booleans
		let mut res = ba.to_owned();
		res.extend_from_bitslice(bb);
		Ok(B(res))
	},

	N(ra), N(rb), _ => Ok(N(ra + rb)),	//add numbers
	
	S(sa), S(sb), _ => Ok(S(sa.to_owned() + sb))	//concat strings
);

dya!(sub
	B(ba), B(bb), _ => {	//boolean xor
		Ok(B(ba.to_owned() ^ bb))
	},

	B(ba), N(rb), _ => {	//shorten boolean
		let ub = r_u(rb)?;
		let mut bz = ba.to_owned();
		bz.truncate(bz.len().saturating_sub(ub));
		Ok(B(bz))
	},

	N(ra), N(rb), _ => Ok(N(ra - rb)),	//subtract numbers

	S(sa), N(rb), _ => {	//shorten string
		let ub = r_u(rb)?;
		let mut i = sa.chars();
		for _ in 0..ub { i.next_back(); }
		Ok(S(i.collect()))
	}
);

dya!(mul
	B(ba), B(bb), _ => {	//boolean and
		Ok(B(ba.clone() & bb))
	},

	B(ba), N(rb), _ => {	//repeat boolean
		let ub = r_u(rb)?;
		if ba.len().checked_mul(ub).is_some() {
			Ok(B(ba.repeat(ub)))
		}
		else {
			Err(Arith(format!("Boolean repeated {ub} times is unrepresentable")))
		}
	},

	N(ra), N(rb), _ => Ok(N(ra * rb)),	//multiply numbers
	
	S(sa), N(rb), _ => {	//repeat string
		let ub = r_u(rb)?;
		if sa.len().checked_mul(ub).is_some() {
			Ok(S(sa.repeat(ub)))
		}
		else {
			Err(Arith(format!("String repeated {ub} times is unrepresentable")))
		}
	}
);

dya!(div
	B(ba), B(bb), _ => {	//boolean or
		Ok(B(ba.clone() | bb))
	},

	B(ba), N(rb), _ => {	//truncate boolean
		let ub = r_u(rb)?;
		let mut bz = ba.to_owned();
		bz.truncate(ub);
		Ok(B(bz))
	},
	
	N(ra), N(rb), _ => {	//divide numbers
		if *rb != Rational::ZERO {
			Ok(N(ra / rb))
		}
		else {	//undefined
			Err(Arith("Division by 0".into()))
		}
	},

	S(sa), N(rb), _ => {	//truncate string
		let ub = r_u(rb)?;
		Ok(S(sa.chars().take(ub).collect()))
	}
);

mon!(neg
	B(ba), _ => Ok(B(!ba.to_owned())),	//negate boolean
	
	N(ra), _ => Ok(N(ra.reciprocal())),	//reciprocate number

	S(sa), _ => {	//invert case of string
		let mut sz = String::new();
		for c in sa.chars() {
			if c.is_uppercase() {
				for l in c.to_lowercase() { sz.push(l); }
			}
			else if c.is_lowercase() {	//cases are mutually exclusive per the Unicode standard
				for u in c.to_uppercase() { sz.push(u); }
			}
			else {
				sz.push(c);
			}
		}
		Ok(S(sz))
	}
);

dya!(pow
	B(ba), B(bb), _ => {	//find sequence in boolean
		if bb.is_empty() {	//empty pattern matches at start
			Ok(N(Rational::ZERO))
		}
		else {
			Ok(N(
				ba.windows(bb.len())
				.position(|bs| bs == bb)
				.map(|uz| uz.into()).unwrap_or(Rational::NEGATIVE_ONE)
			))
		}
	},

	N(ra), N(rb), _ => {	//raise number to power
		if *ra == Rational::ZERO && *rb < Rational::ZERO {	//undefined
			Err(Arith("Negative power of 0".into()))
		}
		else if let Ok(ub) = u64::try_from(&rb.abs()) {
			if *rb < Rational::ZERO {
				Ok(N(ra.reciprocal().pow(ub)))
			}
			else {
				Ok(N(ra.pow(ub)))
			}
		}
		else {
			let (fa, fb) = (r_f(ra)?, r_f(rb)?);
			f_r(fa.powf(fb)).map(N)
		}
	},
	
	S(sa), S(sb), false => {	//find substring by literal
		if let Some(bidx) = sa.find(sb) {	//byte position
			Ok(N(sa.char_indices().position(|(cidx, _)| cidx == bidx).unwrap().into()))	//char position
		}
		else {
			Ok(N(Rational::NEGATIVE_ONE))
		}
	},
	
	S(sa), S(sb), true => {	//find substring by regex
		let re = RE_CACHE.get(sb).map_err(Custom)?;
		if let Some(m) = re.find(sa) {	//find match
			Ok(A(vec![
				N(sa.char_indices().position(|(cidx, _)| cidx == m.start()).unwrap().into()),	//char position of start
				N(m.as_str().chars().count().into())	//char length of match
			]))
		}
		else {
			Ok(A(vec![
				N(Rational::NEGATIVE_ONE),
				N(Rational::ZERO)
			]))
		}
	}
);

mon!(sqrt
	B(ba), _ => {	//reverse boolean
		let mut bz = ba.to_owned();
		bz.reverse();
		Ok(B(bz))
	},

	N(ra), _ => {	//square root of number
		if *ra < Rational::ZERO {	//undefined
			Err(Arith("Square root of negative number".into()))
		}
		else if let Some(rz) = ra.checked_sqrt() {
			Ok(N(rz))
		}
		else {
			f_r(r_f(ra)?.sqrt()).map(N)
		}
	},

	S(sa), _ => Ok(S(sa.chars().rev().collect()))	//reverse string
);

dya!(root
	N(ra), N(rb), _ => {	//bth root of a
		if *rb == Rational::ZERO {	//undefined
			Err(Arith("0th root".into()))
		}
		else if *ra == Rational::ZERO && *rb < Rational::ZERO {
			Err(Arith("Negative root of 0".into()))
		}
		else if let Ok(ub) = u64::try_from(&rb.abs()) {
			if *ra < Rational::ZERO && ub % 2 == 0 {
				Err(Arith("Even root of negative number".into()))
			}
			else if let Some(rz) = ra.checked_root(ub) {
				if *rb < Rational::ZERO {
					Ok(N(rz.reciprocal()))
				}
				else {
					Ok(N(rz))
				}
			}
			else {
				let (fa, fb) = (r_f(ra)?, r_f(rb)?);
				f_r(fa.powf(fb.recip())).map(N)
			}
		}
		else {
			let (fa, fb) = (r_f(ra)?, r_f(rb)?);
			f_r(fa.powf(fb.recip())).map(N)
		}
	}
);

mon!(log
	B(ba), _ => Ok(N(ba.len().into())),	//length of boolean
	
	N(ra), _ => {
		if *ra <= Rational::ZERO {	//undefined
			Err(Arith("Natural logarithm of non-positive number".into()))
		}
		else {
			f_r(ra.approx_log()).map(N)
		}
	},
	
	S(sa), false => Ok(N(sa.chars().count().into())),	//char length of string
	
	S(sa), true => Ok(N(sa.len().into()))	//byte length of string
);

dya!(logb
	B(ba), B(bb), _ => {	//count matches in boolean
		if bb.is_empty() {	//empty pattern matches everywhere
			Ok(N((ba.len() + 1).into()))
		}
		else {
			Ok(N(
				ba.windows(bb.len())
				.filter(|bs| bs == bb).count().into()
			))
		}
	},

	N(ra), N(rb), _ => {	//base b log of a
		if *ra <= Rational::ZERO {
			Err(Arith("Logarithm of non-positive number".into()))
		}
		else if *rb == Rational::ONE {
			Err(Arith("Logarithm with base 1".into()))
		}
		else if let Some(iz) = ra.checked_log_base(rb) {
			Ok(N(iz.into()))
		}
		else {
			f_r(ra.approx_log() / rb.approx_log()).map(N)
		}
	},

	S(sa), S(sb), false => {	//count literal matches in string
		let len = sb.chars().count();
		if len == 0 {
			Ok(N((len + 1).into()))
		}
		else {
			Ok(N(
				sa.as_bytes().windows(len)
				.filter(|bs| *bs == sb.as_bytes()).count().into()
			))
		}
	},

	S(sa), S(sb), true => {	//count regex matches in string
		let re = RE_CACHE.get(sb).map_err(Custom)?;
		Ok(N(re.find_iter(sa).count().into()))
	}
);

dya!(modu
	B(ba), N(rb), _ => {	//extract bit
		let ub = r_u(rb)?;
		if let Some(b) = ba.get(ub) {
			let mut bz = BitVec::new();
			bz.push(*b);
			Ok(B(bz))
		}
		else {
			Err(Index(ub))
		}
	},

	N(ra), N(rb), _ => {	//modulo
		let (ia, ib) = (r_i(ra)?, r_i(rb)?);
		if ib == Rational::ZERO {	//undefined
			Err(Arith("Reduction mod 0".into()))
		}
		else {
			Ok(N(ia.mod_op(ib).into()))
		}
	},

	S(sa), N(rb), _ => {	//extract char
		let ub = r_u(rb)?;
		if let Some(c) = sa.chars().nth(ub) {
			Ok(S(c.into()))
		}
		else {
			Err(Index(ub))
		}
	}
);

dya!(euc
	B(ba), N(rb), _ => {	//split boolean
		let ub = r_u(rb)?;
		let mut i = ba.iter();
		Ok(A(vec![
			B(i.by_ref().take(ub).collect()),
			B(i.collect())
		]))
	},

	N(ra), N(rb), _ => {	//euclidean division
		let (ia, ib) = (r_i(ra)?, r_i(rb)?);
		if ib == Rational::ZERO {	//undefined
			Err(Arith("Euclidean division by 0".into()))
		}
		else {
			let (quot, rem) = ia.div_mod(ib);
			Ok(A(vec![
				N(quot.into()),
				N(rem.into())
			]))
		}
	},

	S(sa), N(rb), _ => {	//split string at char
		let ub = r_u(rb)?;
		let mut i = sa.chars();
		Ok(A(vec![
			S(i.by_ref().take(ub).collect()),
			S(i.collect())
		]))
	}
);

tri!(bar
	a, b, B(bc), _ => {	//selection (c ? a : b)
		if bc.len() == 1 {	//one bit, make scalar
			if bc[0] {Ok(a.to_owned())} else {Ok(b.to_owned())}
		}
		else {	//array of values for each bit
			Ok(A(
				bc.iter().by_vals().map(|bit| if bit {a.to_owned()} else {b.to_owned()}).collect()
			))
		}
	},

	N(ra), N(rb), N(rc), _ => {	//modular exponentiation (a ^ b mod c)
		let mut na = r_n(ra)?;
		let nb = r_n(&rb.abs())?;
		let nc = r_n(rc)?;
		if *rb < Rational::ZERO {	//find inverse if exponent is negative
			let rem = &na % &nc;
			if rem == Natural::ZERO {
				return Err(Arith("0 can't be coprime".into()));
			}
			else {
				na = (&rem).mod_inverse(&nc).ok_or_else(|| Arith(format!("{na} doesn't have a coprime mod {nc}")))?;
			}
		}
		Ok(N((na % &nc).mod_pow(nb, nc).into()))
	},
	
	S(sa), S(sb), S(sc), false => Ok(S(sa.replace(sb, sc))),	//replace substrings by literal
	
	S(sa), S(sb), S(sc), true => {	//replace substrings by regex
		let re = RE_CACHE.get(sb).map_err(Custom)?;
		Ok(S(re.replace_all(sa, sc).into()))
	}
);

mon!(disc
	B(_), _ => Ok(N(Rational::const_from_unsigned(1))),
	N(_), _ => Ok(N(Rational::const_from_unsigned(2))),
	S(_), _ => Ok(N(Rational::const_from_unsigned(3)))
);

dya!(eq
	B(ba), B(bb), _ => {
		if ba.len() == bb.len() {
			let mut bz = BitVec::new();
			bz.push(ba == bb);
			Ok(B(bz))
		}
		else {
			Ok(B(bitvec![u8, Lsb0; 0]))
		}
	},
	N(ra), N(rb), _ => {
		let mut bz = BitVec::new();
		bz.push(ra == rb);
		Ok(B(bz))
	},
	S(sa), S(sb), _ => {
		let mut bz = BitVec::new();
		bz.push(sa == sb);
		Ok(B(bz))
	},
	_, _, true => {	//total: false if types differ
		Ok(B(bitvec![u8, Lsb0; 0]))
	}
);

dya!(lt
	B(ba), B(bb), _ => {
		let mut bz = BitVec::new();
		bz.push(
			if ba.len() == bb.len() {ba < bb}
			else {ba.len() < bb.len()}
		);
		Ok(B(bz))
	},
	N(ra), N(rb), _ => {
		let mut bz = BitVec::new();
		bz.push(ra < rb);
		Ok(B(bz))
	},
	S(sa), S(sb), _ => {
		let mut bz = BitVec::new();
		bz.push(str_cmp(sa, sb).is_lt());
		Ok(B(bz))
	},
	_, _, true => {	//total: false if types differ
		Ok(B(bitvec![u8, Lsb0; 0]))
	}
);

dya!(gt
	B(ba), B(bb), _ => {
		let mut bz = BitVec::new();
		bz.push(
			if ba.len() == bb.len() {ba > bb}
			else {ba.len() > bb.len()}
		);
		Ok(B(bz))
	},
	N(ra), N(rb), _ => {
		let mut bz = BitVec::new();
		bz.push(ra > rb);
		Ok(B(bz))
	},
	S(sa), S(sb), _ => {
		let mut bz = BitVec::new();
		bz.push(str_cmp(sa, sb).is_gt());
		Ok(B(bz))
	},
	_, _, true => {	//total: false if types differ
		Ok(B(bitvec![u8, Lsb0; 0]))
	}
);

mon!(fac
	N(ra), _ => {	//factorial
		let na = r_n(ra)?;
		if let Ok(ua) = u64::try_from(&na) {
			Ok(N(Natural::factorial(ua).into()))
		}
		else {
			Err(Arith(format!("Factorial of {na} is unrepresentable")))
		}
	},

	S(sa), _ => {	//selected constants
		Ok(N(f_r(
			match sa.as_str() {
				"e" => std::f64::consts::E,
				"pi" => std::f64::consts::PI,
				"phi" => 1.618033988749895,
				"gamma" => 0.5772156649015329,
				"delta" => 4.669201609102991,
				"alpha" => 2.5029078750958928,
				"epsilon" => f64::EPSILON,
				_ => {return Err(Arith(format!("Unknown constant {sa}")));}
			}
		)?))
	}
);

dya!(trig
	N(ra), N(rb), _ => {
		let ib = r_i(rb)?;
		match i8::try_from(&ib) {
			Ok(1) => {
				Ok(N(f_r(r_f(ra)?.sin())?))
			},
			Ok(2) => {
				Ok(N(f_r(r_f(ra)?.cos())?))
			},
			Ok(3) => {
				Ok(N(f_r(r_f(ra)?.tan())?))
			},
			Ok(4) => {
				Ok(N(f_r(r_f(ra)?.sinh())?))
			},
			Ok(5) => {
				Ok(N(f_r(r_f(ra)?.cosh())?))
			},
			Ok(6) => {
				Ok(N(f_r(r_f(ra)?.tanh())?))
			},
			Ok(-1) => {
				Ok(N(f_r(r_f(ra)?.asin())?))
			},
			Ok(-2) => {
				Ok(N(f_r(r_f(ra)?.acos())?))
			},
			Ok(-3) => {
				Ok(N(f_r(r_f(ra)?.atan())?))
			},
			Ok(-4) => {
				Ok(N(f_r(r_f(ra)?.asinh())?))
			},
			Ok(-5) => {
				Ok(N(f_r(r_f(ra)?.acosh())?))
			},
			Ok(-6) => {
				Ok(N(f_r(r_f(ra)?.atanh())?))
			},
			_ => {
				Err(Arith(format!("Unknown function number {ib}")))
			}
		}
	}
);