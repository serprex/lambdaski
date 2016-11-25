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
	type Out: F;
}
impl<T: F> A<T> for S {
	type Out = S1<T>;
}
impl<X: F, T: F> A<T> for S1<X> {
	type Out = S2<X, T>;
}
impl<X: F, Y: F, T: F> A<T> for S2<X, Y>
	where X: A<T>, Y: A<T>, <X as A<T>>::Out: A<<Y as A<T>>::Out>
{
	type Out = <<X as A<T>>::Out as A<<Y as A<T>>::Out>>::Out;
}

impl<T: F> A<T> for K {
	type Out = K1<T>;
}
impl<X: F, T: F> A<T> for K1<X> {
	type Out = X;
}

impl<T: F> A<T> for I {
	type Out = T;
}