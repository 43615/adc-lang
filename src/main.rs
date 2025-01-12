//! Executable CLI wrapper

use adc_lang::*;
use adc_lang::structs::Value;
use std::time::{Instant, Duration};
use std::mem::ManuallyDrop;

fn main() {
	let mut foo = Value::A(vec![Value::B(false)]);
	let t0 = Instant::now();
	for i in 1..=300000000 {
		foo = Value::A(vec![foo]);
		if i%10000 == 0 {println!("{i}");}
	}
	let foo1 = ManuallyDrop::new(foo);
	let t1 = Instant::now();
	println!("calling");
	//println!("{:?}", foo);
	let bar = fns::exec1(fns::inv, &foo1, false).unwrap();
	let bar1 = ManuallyDrop::new(bar);
	//println!("{:?}", bar);
	let t2 = Instant::now();
	println!("done");
	println!("{:?} {:?}", t1-t0, t2-t1);
	//std::mem::forget(foo);
	//std::mem::forget(bar);
}