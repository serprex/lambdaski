pub mod typenum3;

use std::marker::PhantomData;

pub struct S;
pub struct K;
pub struct I;
pub struct S1<X>(pub PhantomData<X>);
pub struct S2<X, Y>(pub PhantomData<(X, Y)>);
pub struct K1<X>(pub PhantomData<X>);

pub trait A<In> {
	type O;
}
impl<T> A<T> for S {
	type O = S1<T>;
}
impl<X, T> A<T> for S1<X> {
	type O = S2<X, T>;
}
impl<X, Y, T> A<T> for S2<X, Y>
	where X: A<T>, Y: A<T>, <X as A<T>>::O: A<<Y as A<T>>::O>
{
	type O = <<X as A<T>>::O as A<<Y as A<T>>::O>>::O;
}

impl<T> A<T> for K {
	type O = K1<T>;
}
impl<X, T> A<T> for K1<X> {
	type O = X;
}

impl<T> A<T> for I {
	type O = T;
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn it_works() {
		//let _: <<S as A<I>>::O as A<I>>::O;
		//let _: <<<S as A<I>>::O as A<I>>::O as A<<<S as A<I>>::O as A<I>>::O>>::O;
		let _: <<<S as A<K>>::O as A<I>>::O as A<K>>::O;
	}
}
