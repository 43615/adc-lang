//! Number output functions

use std::cmp::Ordering::*;
use malachite::{Natural, Rational};
use malachite::base::num::arithmetic::traits::{Parity, Pow, Reciprocal, Sign};
use malachite::base::num::basic::traits::{Zero, One, Two};
use malachite::base::num::conversion::traits::{Digits, WrappingFrom};
use malachite::rational::arithmetic::traits::Approximate;

///negative, integer, fractional, recurring
pub fn digits(r: &Rational, k: usize, o: &Natural) -> (bool, Vec<Natural>, Vec<Natural>, Vec<Natural>) {
	let sign = r.sign() == Less;
	let (mut ipart, temp) = r.to_digits(o);
	ipart.reverse();	//malachite puts them in reverse order
	let (mut fpart, mut rpart) = temp.into_vecs();
	if k == 0 || k >= ipart.len()+fpart.len()+rpart.len() {	//all digits fit
		// do nothing
	}
	else if k >= ipart.len()+fpart.len() {	//not all recurring digits fit
		//move the digits that fit into fractionals
		rpart.truncate(1 + k - (ipart.len()+fpart.len()));
		let next = fpart.pop().unwrap();	//first discarded digit
		fpart.append(&mut rpart);
		let ord = (next * Natural::TWO).cmp(o);
		if ord != Less {	//round remaining digits if next >= ceil(o/2)
			let mut to_even = ord == Equal;	//round last digit to even if next == o/2
			for d in fpart.iter_mut().rev().chain(ipart.iter_mut().rev()) {	//carry addition all the way up
				if !to_even || d.odd() {	//only increment last digit if it's odd
					*d += Natural::ONE;	//reused for carrying up after a continue
				}
				to_even = false;
				if d == o {	//roll over
					*d = Natural::ZERO;
					continue;
				}
				else {
					break;
				}
			}
			if ipart[0] == Natural::ZERO {	//if first digit rolled over (r == o^n - 1)
				ipart.insert(0, Natural::ONE);	//add leading 1
			}
		}
		while let Some(&Natural::ZERO) = fpart.last() {	//remove trailing zeros
			fpart.pop();
		}
	}
	else if k >= ipart.len() {	//not all fractional digits fit
		rpart.clear();
		fpart.truncate(1 + k - ipart.len());
		let next = fpart.pop().unwrap();	//first discarded digit
		let ord = (next * Natural::TWO).cmp(o);
		if ord != Less {	//round remaining digits if next >= ceil(o/2)
			let mut to_even = ord == Equal;	//round last digit to even if next == o/2
			for d in fpart.iter_mut().rev().chain(ipart.iter_mut().rev()) {	//carry addition all the way up
				if !to_even || d.odd() {	//only increment last digit if it's odd
					*d += Natural::ONE;	//reused for carrying up after a continue
				}
				to_even = false;
				if d == o {	//roll over
					*d = Natural::ZERO;
					continue;
				}
				else {
					break;
				}
			}
			if ipart[0] == Natural::ZERO {	//if first digit rolled over (r == o^n - 1)
				ipart.insert(0, Natural::ONE);	//add leading 1
			}
		}
		while let Some(&Natural::ZERO) = fpart.last() {	//remove trailing zeros
			fpart.pop();
		}
	}
	else {	//not all integer digits fit
		rpart.clear();
		fpart.clear();
		let mut len = ipart.len();
		ipart.truncate(1 + k);
		let next = ipart.pop().unwrap();	//first discarded digit
		let ord = (next * Natural::TWO).cmp(o);
		if ord != Less {	//round remaining digits if next >= ceil(o/2)
			let mut to_even = ord == Equal;	//round last digit to even if next == o/2
			for d in ipart.iter_mut().rev() {	//carry addition all the way up
				if !to_even || d.odd() {	//only increment last digit if it's odd
					*d += Natural::ONE;	//reused for carrying up after a continue
				}
				to_even = false;
				if d == o {	//roll over
					*d = Natural::ZERO;
					continue;
				}
				else {
					break;
				}
			}
			if ipart[0] == Natural::ZERO {	//if first digit rolled over (r == o^n - 1)
				ipart.insert(0, Natural::ONE);	//add leading 1
				len += 1;	//adjust target length
			}
		}
		ipart.resize(len, Natural::ZERO);	//fill with zeros to original length
	}

	(sign, ipart, fpart, rpart)
}

///digit to character byte: 0-9a-z
fn chr(d: &Natural) -> u8 {
	let u = u8::wrapping_from(d);
	if u < 10 { u + 48 } else { u + 87 }
}

///normal notation from digits
pub fn nnorm(neg: bool, ipart: &[Natural], fpart: &[Natural], rpart: &[Natural], o: &Natural) -> String {
	let mut res = Vec::new();
	if *o > Natural::const_from(36) {	//enclosed any-base format
		res.push(b'\'');	//prefix for any-base

		if neg {
			res.push(b'`');	//negative sign
		}

		if ipart.is_empty() {
			res.push(b'0');	//leading zero
			res.push(b' ');
		}
		else {
			for id in ipart {
				res.extend_from_slice(id.to_string().as_bytes());	//integer digits
				res.push(b' ');
			}
		}

		if !fpart.is_empty() {
			*res.last_mut().unwrap() = b'.';	//fractional point
			for fd in fpart {
				res.extend_from_slice(fd.to_string().as_bytes());	//fractional digits
				res.push(b' ');
			}
		}

		if !rpart.is_empty() {
			if fpart.is_empty() {
				res.push(b'.');
				res.push(b'`');
			}
			else {
				*res.last_mut().unwrap() = b'`';	//recurring prefix
			}
			for rd in rpart {
				res.extend_from_slice(rd.to_string().as_bytes());	//recurring digits
				res.push(b' ');
			}
		}

		*res.last_mut().unwrap() = b'\'';	//suffix for any-base
	}
	else {	//low base
		if *o > Natural::const_from(10) {
			res.push(b'\'');	//prefix for bases 11-36
		}

		if neg {
			res.push(b'`');	//negative sign
		}

		if ipart.is_empty() {
			res.push(b'0');	//leading zero
		}
		else {
			for id in ipart {
				res.push(chr(id));	//integer digits
			}
		}

		if !fpart.is_empty() {
			res.push(b'.');	//fractional point
			for fd in fpart {
				res.push(chr(fd));	//fractional digits
			}
		}

		if !rpart.is_empty() {
			if fpart.is_empty() {
				res.push(b'.');
			}
			res.push(b'`');	//recurring prefix
			for rd in rpart {
				res.push(chr(rd));	//recurring digits
			}
		}
	}
	unsafe { String::from_utf8_unchecked(res) }	//only ASCII
}

///scientific notation from digits
pub fn nsci(neg: bool, ipart: &[Natural], fpart: &[Natural], rpart: &[Natural], o: &Natural) -> String {
	if ipart.is_empty() {	//negative exponent
		if let Some(pos) = fpart.iter().position(|n| *n != Natural::ZERO) {    //find first nonzero fractional digit
			let (inew, fnew) = fpart.split_at(pos).1.split_at(1);
			let mut res = nnorm(neg, inew, fnew, rpart, o);
			if *o > Natural::const_from(36) {
				res.pop();
				res += &format!("@`{}'", pos+1);
			}
			else {
				res += &format!("@`{}", pos+1);
			}
			res
		}
		else {	//no nonzero fractional digits, only recurring
			let exp = fpart.len();
			let mut res = nnorm(neg, ipart, &[], rpart, o);
			if *o > Natural::const_from(36) {
				res.pop();
				res += &format!("@`{exp}'");
			}
			else {
				res += &format!("@`{exp}");
			}
			res
		}
	}
	else {	//positive exponent
		let (inew, fpre) = ipart.split_at(1);	//keep first integer digit
		let exp = fpre.len();	//amount of moved digits
		let mut fnew = [fpre, fpart].concat();
		while let Some(&Natural::ZERO) = fnew.last() {	//remove trailing zeros (possibly added from ipart if fpart is empty)
			fnew.pop();
		}
		let mut res = nnorm(neg, inew, &fnew, rpart, o);
		if *o > Natural::const_from(36) {
			res.pop();
			res += &format!("@{exp}'");
		}
		else {
			res += &format!("@{exp}");
		}
		res
	}
}

///fractional notation from original
pub fn nfrac(r: &Rational, k: usize, o: &Natural) -> String {
	let (mut numer, mut denom) = r.to_numerator_and_denominator();

	if k != 0 {
		let max = o.pow(k as u64);
		if numer > denom {
			(denom, numer) = r.reciprocal().approximate(&max).to_numerator_and_denominator();
		}
		else {
			(numer, denom) = r.approximate(&max).to_numerator_and_denominator();
		}
	}
	
	let mut res = Vec::new();
	
	let mut sign = r.sign() == Less;
	
	#[allow(clippy::tuple_array_conversions)]
	for n in [numer, denom] {
		if *o > Natural::const_from(10) {
			res.push(b'\'');	//high base prefix
		}

		if sign {
			res.push(b'`');	//add sign to numerator
			sign = false;
		}

		let mut digits = n.to_digits_desc(o);
		if digits.is_empty() {
			digits.push(Natural::ZERO);
		}

		if *o > Natural::const_from(36) {	//any-base
			for d in digits {
				res.extend_from_slice(d.to_string().as_bytes());	//add digit values
				res.push(b' ');
			}
			*res.last_mut().unwrap() = b'\'';	//any-base suffix
		}
		else {	//low base
			for d in digits {
				res.push(chr(&d));	//add digit values
			}
		}

		res.push(b' ');
	}

	*res.last_mut().unwrap() = b'/';

	unsafe { String::from_utf8_unchecked(res) }	//only ASCII
}

///shortest of nnorm, nsci, nfrac (in this order)
pub fn nauto(r: &Rational, k: usize, o: &Natural) -> String {
	let (neg, ipart, fpart, rpart) = digits(r, k, o);
	let norm = nnorm(neg, &ipart, &fpart, &rpart, o);
	let sci = nsci(neg, &ipart, &fpart, &rpart, o);
	let frac = nfrac(r, k, o);
	if norm.len() <= sci.len() && norm.len() <= frac.len() {
		norm
	}
	else if sci.len() <= frac.len() {
		sci
	}
	else {
		frac
	}
}