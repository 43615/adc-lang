//! Number output functions

use std::cmp::Ordering::*;
use malachite::{Natural, Rational};
use malachite::base::num::arithmetic::traits::*;
use malachite::base::num::basic::traits::{Zero, One, Two};
use malachite::base::num::conversion::traits::{Digits, WrappingFrom};
use malachite::rational::rational::arithmetic::traits::Approximate;

///ascending significance, ties to even
fn round<'a>(digits: impl Iterator<Item = &'a mut Natural>, next: Natural, o: &Natural) {
	let ord = (next * Natural::TWO).cmp(o);
	if ord != Less {	//round remaining digits if next >= ceil(o/2)
		let mut to_even = ord == Equal;	//round last digit to even if next == o/2
		for d in digits {	//carry addition all the way up
			if !to_even || d.odd() {	//only increment last digit if it's odd
				*d += Natural::ONE;	//reused for carrying up after a continue
			}
			to_even = false;
			if d == o {	//roll over
				*d = Natural::ZERO;
				continue;
			}
			else {	//rounding finished
				break;
			}
		}
	}
}

///negative, integer, fractional, recurring
pub fn digits(r: &Rational, k: usize, o: &Natural) -> (bool, Vec<Natural>, Vec<Natural>, Vec<Natural>) {
	if *o == Natural::ONE {return factoradic_digits(r, k);}	//automatic fallback
	let sign = r.sign() == Less;
	if k == 0 {	//unlimited precision, just compute all digits
		let (mut ipart, temp) = r.to_digits(o);
		ipart.reverse();	//malachite puts them in reverse order
		let (fpart, rpart) = temp.into_vecs();
		(sign, ipart, fpart, rpart)
	}
	else {
		let (mut ipart, mut digits) = r.foer_digits(o);
		ipart.reverse();	//malachite puts them in reverse order
		let (mut fpart, mut rpart) = (Vec::new(), Vec::new());
		if let Some(k) = k.checked_sub(ipart.len()) {	//ipart fits
			let mut period = false;
			let mut whole_rpart = false;
			for fd in digits.by_ref().take(k) {
				if fd.period_start {
					if period {whole_rpart = true; break;}
					else {period = true;}
				}
				if fd.repeating {rpart.push(fd.digit);} else {fpart.push(fd.digit);}
			}
			if let Some(next) = digits.next() {	//whole fpart + rpart didn't fit in k
				if !whole_rpart && !next.period_start {    //whole rpart didn't fit in k
					fpart.append(&mut rpart);    //move incomplete rpart into fpart
				}
				if rpart.is_empty() {	//if fpart is the end
					round(fpart.iter_mut().rev().chain(ipart.iter_mut().rev()), next.digit, o);
					if ipart[0] == Natural::ZERO {    //if first digit rolled over (r == o^n - 1):
						ipart.insert(0, Natural::ONE);    //add leading 1
					}
					while let Some(&Natural::ZERO) = fpart.last() {    //remove trailing zeros
						fpart.pop();
					}
				}
			}
		}
		else {	//ipart doesn't fit
			let mut len = ipart.len();
			ipart.truncate(k + 1);
			let next = unsafe { ipart.pop().unwrap_unchecked() };
			round(ipart.iter_mut().rev(), next, o);
			if ipart[0] == Natural::ZERO {	//if first digit rolled over (r == o^n - 1):
				ipart.insert(0, Natural::ONE);	//add leading 1
				len += 1;	//adjust target length
			}
			ipart.resize(len, Natural::ZERO);	//fill with zeros to original length
		}
		(sign, ipart, fpart, rpart)
	}
}

///rpart is always empty, just preserves the signature
pub fn factoradic_digits(r: &Rational, k: usize) -> (bool, Vec<Natural>, Vec<Natural>, Vec<Natural>) {
	let sign = r.sign() == Less;
	let (temp, mut digits) = r.factoradic_digits();
	let mut ipart = temp.into_iter().rev().map(Natural::from).collect();
	if k == 0 {    //unlimited precision, just compute all digits
		let fpart = digits.map(Natural::from).collect();
		(sign, ipart, fpart, Vec::new())
	}
	else {
		let mut fpart = Vec::new();
		if let Some(k) = k.checked_sub(ipart.len()) {	//ipart fits
			fpart.extend(digits.by_ref().take(k).map(Natural::from));
			if let Some(next) = digits.next().map(Natural::from) {	//whole fpart didn't fit in k
				let mut place = k + 2;	//"base" (max+1) of current place
				let mut inc = false;
				let ord = unsafe { (next * Natural::TWO).partial_cmp(&place).unwrap_unchecked() };
				if ord != Less {	//round remaining digits if next >= ceil(place/2)
					let mut to_even = ord == Equal;	//round last digit to even if next == place/2
					for d in fpart.iter_mut().rev().chain(ipart.iter_mut().rev()) {	//carry addition all the way up
						if !to_even || d.odd() {	//only increment last digit if it's odd
							*d += Natural::ONE;	//reused for carrying up after a continue
						}
						to_even = false;
						if *d == place {	//roll over
							*d = Natural::ZERO;
							if inc {place += 1;}	//adjust for next digit
							else if place == 2 {inc = true;}	//passed into ipart, turn around
							else {place -= 1;}
							continue;
						}
						else {
							break;
						}
					}
					if ipart[0] == Natural::ZERO {	//if first digit rolled over
						ipart.insert(0, Natural::ONE);	//add leading 1
					}
				}
				while let Some(&Natural::ZERO) = fpart.last() {	//remove trailing zeros
					fpart.pop();
				}
			}
		}
		else {	//ipart doesn't fit
			let mut len = ipart.len();
			ipart.truncate(k + 1);
			let mut place = len - k + 2;	//"base" (max+1) of current place
			let next = unsafe { ipart.pop().unwrap_unchecked() };	//first discarded digit
			let ord = unsafe { (next * Natural::TWO).partial_cmp(&place).unwrap_unchecked() };
			if ord != Less {	//round remaining digits if next >= ceil(place/2)
				let mut to_even = ord == Equal;	//round last digit to even if next == place/2
				for d in ipart.iter_mut().rev() {	//carry addition all the way up
					if !to_even || d.odd() {	//only increment last digit if it's odd
						*d += Natural::ONE;	//reused for carrying up after a continue
					}
					to_even = false;
					if *d == place {	//roll over
						*d = Natural::ZERO;
						place += 1;	//adjust for next digit
						continue;
					}
					else {
						break;
					}
				}
				if ipart[0] == Natural::ZERO {	//if first digit rolled over
					ipart.insert(0, Natural::ONE);	//add leading 1
					len += 1;	//adjust target length
				}
			}
			ipart.resize(len, Natural::ZERO);	//fill with zeros to original length
		}
		(sign, ipart, fpart, Vec::new())
	}
}

///digit to character byte: 0-9a-z
#[inline(always)] fn chr(d: &Natural) -> u8 {
	let u = u8::wrapping_from(d);
	if u < 10 { u + 0x30 } else { u + 0x57 }
}

///normal notation from digits
pub fn nnorm(neg: bool, ipart: &[Natural], fpart: &[Natural], rpart: &[Natural], o: &Natural) -> String {
	let mut res = Vec::new();
	if *o > Natural::const_from(36) || *o == Natural::ONE {	//enclosed any-base/factoradic format
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
			unsafe { *res.last_mut().unwrap_unchecked() = b'.'; }	//fractional point
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
				unsafe { *res.last_mut().unwrap_unchecked() = b'`'; }	//recurring prefix
			}
			for rd in rpart {
				res.extend_from_slice(rd.to_string().as_bytes());	//recurring digits
				res.push(b' ');
			}
		}

		unsafe { *res.last_mut().unwrap_unchecked() = b'\''; }	//suffix for any-base
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
			if *o > Natural::const_from(36) || *o == Natural::ONE {
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
			if *o > Natural::const_from(36) || *o == Natural::ONE {
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
		if *o > Natural::const_from(36) || *o == Natural::ONE {
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
	let (numer, denom) = if k == 0 {
		r.to_numerator_and_denominator()
	}
	else {
		let max = if *o != Natural::ONE { o.pow(k as u64) - Natural::ONE }
		else { Natural::factorial(k as u64 + 1) - Natural::ONE };
		r.approximate(&max).into_numerator_and_denominator()
	};
	
	let mut res = Vec::new();
	
	let mut sign = r.sign() == Less;
	
	for n in [numer, denom] {
		if *o > Natural::const_from(10) || *o == Natural::ONE {
			res.push(b'\'');	//high base/factoradic prefix
		}

		if sign {
			res.push(b'`');	//add sign to numerator
			sign = false;
		}

		let mut digits = if *o != Natural::ONE {
			n.to_digits_desc(o)
		}
		else {
			factoradic_digits(&Rational::from(n), k).1
		};
		if digits.is_empty() {
			digits.push(Natural::ZERO);
		}

		if *o > Natural::const_from(36) || *o == Natural::ONE {	//any-base/factoradic
			for d in digits {
				res.extend_from_slice(d.to_string().as_bytes());	//add digit values
				res.push(b' ');
			}
			unsafe { *res.last_mut().unwrap_unchecked() = b'\''; }	//any-base/factoradic suffix
		}
		else {	//low base
			for d in digits {
				res.push(chr(&d));	//add digit values
			}
		}

		res.push(b' ');
	}

	unsafe {
		*res.last_mut().unwrap_unchecked() = b'/';
		String::from_utf8_unchecked(res)	//only ASCII
	}
}

///shortest of nnorm, nsci, nfrac (in this order)
pub fn nauto(r: &Rational, k: usize, o: &Natural) -> String {
	if *o != Natural::ONE {
		let (sign, ipart, fpart, rpart) = digits(r, k, o);
		let norm = nnorm(sign, &ipart, &fpart, &rpart, o);
		let sci = nsci(sign, &ipart, &fpart, &rpart, o);
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
	else {	//for factoradics, only use nnorm unless explicitly selected
		let (sign, ipart, fpart, rpart) = factoradic_digits(r, k);
		nnorm(sign, &ipart, &fpart, &rpart, o)
	}
}