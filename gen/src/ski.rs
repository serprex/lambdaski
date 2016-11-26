use std::collections::HashMap;
use std::str::Chars;
use eval::Eval;

#[derive(Copy, Clone)]
enum Token { S, K, I, OP, CP }
#[derive(Copy, Clone)]
enum C { S, K, I }
enum A {
	Term(C),
	App(Box<(A, A)>),
}

impl A {
	pub fn push_to_string(&self, s: &mut String) {
		match *self {
			A::Term(C::S) => s.push('S'),
			A::Term(C::K) => s.push('K'),
			A::Term(C::I) => s.push('I'),
			A::App(ref a) => {
				s.push('(');
				a.0.push_to_string(s);
				a.1.push_to_string(s);
				s.push(')');
			}
		}
	}
	pub fn to_string(&self) -> String {
		let mut ret = String::new();
		self.push_to_string(&mut ret);
		ret
	}
	pub fn push_rust(&self, s: &mut String) {
		match *self {
			A::Term(C::S) => s.push('S'),
			A::Term(C::K) => s.push('K'),
			A::Term(C::I) => s.push('I'),
			A::App(ref a) => {
				s.push('<');
				a.0.push_rust(s);
				s.push_str(" as A<");
				a.1.push_rust(s);
				s.push_str(">>::O");
			}
		}
	}
}

#[derive(Default)]
pub struct Ski(HashMap<String, A>);
impl Ski {
	fn skip_ws(s: &mut Chars) -> Option<Token> {
		for c in s {
			return Some(match c {
				'S' => Token::S,
				'K' => Token::K,
				'I' => Token::I,
				'(' => Token::OP,
				')' => Token::CP,
				_ => continue,
			})
		}
		None
	}
	fn parse_app0(&mut self, s: &mut Chars) -> Option<A> {
		let a0;
		if let Some(ch) = Ski::skip_ws(s) {
			a0 = match ch {
				Token::S => A::Term(C::S),
				Token::K => A::Term(C::K),
				Token::I => A::Term(C::I),
				Token::OP =>
					if let Some(a0) = self.parse_app(s) {
						a0
					} else {
						return None
					},
				Token::CP => return None,
			}
		} else {
			return None
		}
		Some(a0)
	}
	fn parse_app1(&mut self, a0: A, s: &mut Chars) -> A {
		if let Some(a1) = self.parse_app0(s) {
			self.parse_app1(A::App(Box::new((a0, a1))), s)
		} else {
			a0
		}
	}
	fn parse_app(&mut self, s: &mut Chars) -> Option<A> {
		self.parse_app0(s).map(|a0| self.parse_app1(a0, s))
	}
}
impl Eval for Ski {
	fn line(&mut self, s: &str) -> Option<String> {
		Some(if let Some(expr) = self.parse_app(&mut s.chars()) {
			expr.to_string()
		} else {
			String::new()
		})
	}
	fn set(&mut self, k: String, s: &str) -> Option<String> {
		Some(if let Some(a) = self.parse_app(&mut s.chars()) {
			let ret = a.to_string();
			self.0.insert(k, a);
			ret
		} else {
			String::new()
		})
	}
	fn spit(&self) -> String {
		let mut ret = String::from("use lambdaski::ski::{S,K,I,A};\n");
		for (k, v) in self.0.iter() {
			ret.push_str("type ");
			ret.push_str(k);
			ret.push('=');
			v.push_rust(&mut ret);
			ret.push_str(";\n");
		}
		ret
	}
}
