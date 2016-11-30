use std::marker::PhantomData;

pub struct N;
pub struct Z;
pub struct P;
pub struct I<X, T>(PhantomData<(X, T)>);

pub trait Add<In> {
	type O;
}

impl<X> Add<X> for Z {
	type O = X;
}
impl Add<Z> for N {
	type O = N;
}
impl Add<Z> for P {
	type O = P;
}
impl<X, T> Add<Z> for I<X, T> {
	type O = I<X, T>;
}
impl Add<P> for N {
	type O = Z;
}
impl Add<N> for P {
	type O = Z;
}
impl Add<P> for P {
	type O = I<P, Z>;
}
impl Add<N> for N {
	type O = I<N, Z>;
}
impl<X> Add<P> for I<X, Z> {
	type O = I<X, P>;
}
impl<X> Add<N> for I<X, Z> {
	type O = I<X, N>;
}
impl<X> Add<P> for I<X, P> where X: Add<P> {
	type O = I<<X as Add<P>>::O, N>;
}
impl<X> Add<N> for I<X, P> {
	type O = I<X, Z>;
}
impl<X> Add<P> for I<X, N> {
	type O = I<X, Z>;
}
impl<X> Add<N> for I<X, N> where X: Add<N> {
	type O = I<<X as Add<N>>::O, P>;
}
impl<X> Add<I<X, P>> for P where X: Add<P> {
	type O = I<<X as Add<P>>::O, N>;
}
impl<X> Add<I<X, N>> for P {
	type O = I<X, Z>;
}
impl<X> Add<I<X, Z>> for P {
	type O = I<X, P>;
}
impl<X> Add<I<X, P>> for N {
	type O = I<X, Z>;
}
impl<X> Add<I<X, N>> for N where X: Add<N> {
	type O = I<<X as Add<N>>::O, P>;
}
impl<X> Add<I<X, Z>> for N {
	type O = I<X, N>;
}
impl<X, Y, Yt> Add<I<X, Z>> for I<Y, Yt> where X: Add<Y> {
	type O = I<<X as Add<Y>>::O, Yt>;
}
impl<X, Y> Add<I<X, P>> for I<Y, Z> where X: Add<Y> {
	type O = I<<X as Add<Y>>::O, P>;
}
impl<X, Y> Add<I<X, N>> for I<Y, Z> where X: Add<Y> {
	type O = I<<X as Add<Y>>::O, N>;
}
impl<X, Y> Add<I<X, P>> for I<Y, P> where X: Add<Y>, <X as Add<Y>>::O: Add<P> {
	type O = I<<<X as Add<Y>>::O as Add<P>>::O, N>;
}
impl<X, Y> Add<I<X, N>> for I<Y, P> where X: Add<Y> {
	type O = I<<X as Add<Y>>::O, Z>;
}
impl<X, Y> Add<I<X, P>> for I<Y, N> where X: Add<Y> {
	type O = I<<X as Add<Y>>::O, Z>;
}
impl<X, Y> Add<I<X, N>> for I<Y, N> where X: Add<Y>, <X as Add<Y>>::O: Add<N> {
	type O = I<<<X as Add<Y>>::O as Add<N>>::O, P>;
}

pub trait ToInt {
	fn to_i64() -> i64;
}

impl ToInt for Z {
	fn to_i64() -> i64 {
		0
	}
}
impl ToInt for P {
	fn to_i64() -> i64 {
		1
	}
}
impl ToInt for N {
	fn to_i64() -> i64 {
		-1
	}
}

impl<X: ToInt> ToInt for I<X, Z> {
	fn to_i64() -> i64 {
		X::to_i64() * 3
	}
}

impl<X: ToInt> ToInt for I<X, P> {
	fn to_i64() -> i64 {
		X::to_i64() * 3 + 1
	}
}

impl<X: ToInt> ToInt for I<X, N> {
	fn to_i64() -> i64 {
		X::to_i64() * 3 - 1
	}
}

mod t3sts {
	use super::*;
	#[test]
	fn add() {
		type X7 = I<I<P, N>, P>;
		type X1 = P;
		type XN3 = I<N, Z>;
		assert_eq!(<X7 as Add<X1>>::O::to_i64(), 8);
		assert_eq!(<X1 as Add<X7>>::O::to_i64(), 8);
		assert_eq!(<X7 as Add<XN3>>::O::to_i64(), 4);
		assert_eq!(<XN3 as Add<X7>>::O::to_i64(), 4);
	}
}
