//! Executable CLI wrapper

use adc_lang::*;
use adc_lang::structs::Value;
use std::time::{Instant, Duration};
use bit_vec::BitVec;

fn main() {
	//stop deleting my tab
	use Value::*;
	let mut foo = A(Vec::new());
	let t0 = Instant::now();
	for _ in 0..10 {
		foo = A(vec![foo]);
	}
	let t1 = Instant::now();
	let bar = foo.clone();
	let t2 = Instant::now();
	let baz = foo.clone();
	let t3 = Instant::now();
	println!("{:?}\n{:?}\n{:?}", foo, bar, baz);

	drop(foo);
	let t4 = Instant::now();
	drop(bar);
	let t5 = Instant::now();
	drop(baz);
	let t6 = Instant::now();
	println!("{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}", t1-t0, t2-t1, t3-t2, t4-t3, t5-t4, t6-t5);
}