use std::marker::PhantomData;

pub struct N;
pub struct Z;
pub struct P;
pub struct I<X, T>(PhantomData<(X, T)>);

pub trait NonZero {}
impl NonZero for P {}
impl NonZero for N {}
impl<X: NonZero, T> NonZero for I<X, T> {}

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

pub trait Mul3 {
	type O;
}

impl Mul3 for Z {
	type O = Z;
}
impl Mul3 for P {
	type O = I<P, Z>;
}
impl Mul3 for N {
	type O = I<N, Z>;
}
impl<X, T> Mul3 for I<X, T> {
	type O = I<I<X, T>, Z>;
}

pub trait Mul<In> {
	type O;
}

impl<X> Mul<X> for Z {
	type O = Z;
}
impl<X> Mul<X> for P {
	type O = X;
}
impl Mul<P> for N {
	type O = N;
}
impl Mul<N> for N {
	type O = P;
}
impl Mul<Z> for N {
	type O = Z;
}
impl<X, T> Mul<Z> for I<X, T> {
	type O = Z;
}
impl<X, T> Mul<P> for I<X, T> {
	type O = I<X, T>;
}
impl<X, T> Mul<I<X, T>> for N where X: Mul<N>, T: Mul<N> {
	type O = I<<X as Mul<N>>::O, <T as Mul<N>>::O>;
}
impl<X, T> Mul<N> for I<X, T> where X: Mul<N>, T: Mul<N> {
	type O = I<<X as Mul<N>>::O, <T as Mul<N>>::O>;
}
// Xx * Yy = (X*Y*3 + X*y + Y*x)*3 + x*y
impl<X, Xt, Y, Yt> Mul<I<X, Xt>> for I<Y, Yt>
where Xt: Mul<Yt>,
	X: Mul<Yt>,
	Y: Mul<Xt>,
	X: Mul<Y>,
	<X as Mul<Y>>::O: Mul3,
	<<X as Mul<Y>>::O as Mul3>::O: Add<<Y as Mul<Xt>>::O>,
	<<<X as Mul<Y>>::O as Mul3>::O as Add<<Y as Mul<Xt>>::O>>::O: Add<<X as Mul<Yt>>::O>,
	<<<<X as Mul<Y>>::O as Mul3>::O as Add<<Y as Mul<Xt>>::O>>::O as Add<<X as Mul<Yt>>::O>>::O: Mul3,
	<<<<<X as Mul<Y>>::O as Mul3>::O as Add<<Y as Mul<Xt>>::O>>::O as Add<<X as Mul<Yt>>::O>>::O as Mul3>::O: Add<<Xt as Mul<Yt>>::O>
{
	type O = <<<<<<X as Mul<Y>>::O as Mul3>::O as Add<<Y as Mul<Xt>>::O>>::O as Add<<X as Mul<Yt>>::O>>::O as Mul3>::O as Add<<Xt as Mul<Yt>>::O>>::O;
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

#[cfg(test)]
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
		assert_eq!(<X7 as Mul<X7>>::O::to_i64(), 49);
		assert_eq!(<XN3 as Mul<X7>>::O::to_i64(), -21);
		assert_eq!(<X7 as Mul<XN3>>::O::to_i64(), -21);
	}
}
