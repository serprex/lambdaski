use std::collections::HashMap;
use eval::Eval;

enum Expr {
	Var(String),
	Lam(String, Box<Expr>),
	Apply(Box<(Expr, Expr)>),
}

#[derive(Default)]
pub struct Lambda(HashMap<String, Expr>);
impl Eval for Lambda {
	fn line(&mut self, s: &str) -> Option<String> {
		Some(String::new())
	}
	fn set(&mut self, k: String, s: &str) -> Option<String> {
		Some(String::new())
	}
	fn spit(&self) -> String {
		String::new()
	}
}
