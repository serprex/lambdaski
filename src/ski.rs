use std::marker::PhantomData;

#[derive(Copy, Clone, Debug, Default)]
pub struct S;
#[derive(Copy, Clone, Debug, Default)]
pub struct K;
#[derive(Copy, Clone, Debug, Default)]
pub struct I;
#[derive(Copy, Clone, Debug, Default)]
pub struct S1<X: F>(PhantomData<X>);
#[derive(Copy, Clone, Debug, Default)]
pub struct S2<X: F, Y: F>(PhantomData<(X, Y)>);
#[derive(Copy, Clone, Debug, Default)]
pub struct K1<X: F>(PhantomData<X>);

pub trait F {}
impl F for S {}
impl<T: F> F for S1<T> {}
impl<T: F, U: F> F for S2<T, U> {}
impl F for K {}
impl<T: F> F for K1<T> {}
impl F for I {}

pub trait A<In: F> {
	type O: F;
}
impl<T: F> A<T> for S {
	type O = S1<T>;
}
impl<X: F, T: F> A<T> for S1<X> {
	type O = S2<X, T>;
}
impl<X: F, Y: F, T: F> A<T> for S2<X, Y>
	where X: A<T>, Y: A<T>, <X as A<T>>::O: A<<Y as A<T>>::O>
{
	type O = <<X as A<T>>::O as A<<Y as A<T>>::O>>::O;
}

impl<T: F> A<T> for K {
	type O = K1<T>;
}
impl<X: F, T: F> A<T> for K1<X> {
	type O = X;
}

impl<T: F> A<T> for I {
	type O = T;
}