pub mod ski;

#[cfg(test)]
mod tests {
	use ski::*;
	#[test]
	fn it_works() {
		//let x: <<S as A<I>>::O as A<I>>::O = Default::default();
		//let x: <<<S as A<I>>::O as A<I>>::O as A<<<S as A<I>>::O as A<I>>::O>>::O;
		let x: <<<S as A<K>>::O as A<I>>::O as A<K>>::O = Default::default();
		println!("{:?}", x);
		assert!(false);
	}
}
