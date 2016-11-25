pub mod ski;

#[cfg(test)]
mod tests {
	use ski::*;
	#[test]
	fn it_works() {
		//let x: <<S as A<I>>::Out as A<I>>::Out = Default::default();
		//let x: <<<S as A<I>>::Out as A<I>>::Out as A<<<S as A<I>>::Out as A<I>>::Out>>::Out;
		let x: <<<S as A<K>>::Out as A<I>>::Out as A<K>>::Out = Default::default();
		println!("{:?}", x);
		assert!(false);
	}
}
