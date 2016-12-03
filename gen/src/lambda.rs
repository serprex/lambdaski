use std::fmt;
use fnv::{FnvHashMap, FnvHashSet};
use eval::Eval;

enum Token<'a> {
	Var(usize), L(&'a str), OP, CP,
}

struct RustType {
	pub vars: Vec<usize>,
	pub expr: String,
	pub wher: Vec<String>,
	pub id: usize,
}

impl RustType {
	pub fn new(idcell: &mut usize) -> RustType {
		let id = *idcell;
		*idcell += 1;
		RustType {
			vars: Vec::new(),
			expr: String::new(),
			wher: Vec::new(),
			id: id,
		}
	}
}

enum RustExpr {
	Var(usize),
	L(Box<(RustType, RustExpr)>),
	Apply(Box<(RustExpr, RustExpr)>),
}

impl RustExpr {
	pub fn build_vars(&self, expr: &mut String, wher: &mut Vec<String>) {
		match *self {
			RustExpr::Var(var) => {
				expr.push('X');
				expr.push_str(&var.to_string());
			},
			RustExpr::L(ref bx) => {
				let lrt = &bx.0;
				expr.push('L');
				expr.push_str(&lrt.id.to_string());
				if lrt.vars.len() > 0 {
					expr.push('<');
					for v in lrt.vars.iter() {
						expr.push('X');
						expr.push_str(&(v-1).to_string());
						expr.push(',');
					}
					expr.pop();
					expr.push('>');
				}
			},
			RustExpr::Apply(ref bx) => {
				let mut bx0 = String::new();
				let mut bx1 = String::new();
				bx.0.build_vars(&mut bx0, wher);
				bx.1.build_vars(&mut bx1, wher);
				expr.push('<');
				expr.push_str(&bx0);
				expr.push_str(" as A<");
				expr.push_str(&bx1);
				expr.push_str(">>::O");
				let mut wh = String::new();
				wh.push_str(&bx0);
				wh.push(':');
				wh.push_str("A<");
				wh.push_str(&bx1);
				wh.push('>');
				wher.push(wh);
			},
		}
	}

	pub fn collect(self, types: &mut Vec<RustType>) {
		match self {
			RustExpr::Var(_) => (),
			RustExpr::L(bx) => {
				let tyex = *bx;
				let (ty, ex) = tyex;
				types.push(ty);
				ex.collect(types);
			}
			RustExpr::Apply(bx) => {
				let exs = *bx;
				let (ex0, ex1) = exs;
				ex0.collect(types);
				ex1.collect(types);
			}
		}
	}
}

enum Expr {
	Var(usize),
	L(Box<Expr>),
	Apply(Box<(Expr, Expr)>),
}

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		match *self {
			Expr::Var(id) => write!(f, "{}", id),
			Expr::L(ref expr) => write!(f, "\u{3bb}{}", expr),
			Expr::Apply(ref bx) => write!(f, "({} {})", bx.0, bx.1),
		}
	}
}

impl Expr {
	pub fn to_rust_expr<'a, 'b>(&'a self, scope: &'a mut Vec<FnvHashSet<usize>>, idcell: &'a mut usize) -> RustExpr {
		match *self {
			Expr::Var(x) => {
				rscope_add(scope, x);
				RustExpr::Var(x)
			},
			Expr::L(ref expr) => {
				scope.push(FnvHashSet::default());
				let mut rt = RustType::new(idcell);
				let rte = expr.to_rust_expr(scope, idcell);
				rt.vars = varvec(scope.pop().unwrap());
				rte.build_vars(&mut rt.expr, &mut rt.wher);
				RustExpr::L(Box::new((rt, rte)))
			},
			Expr::Apply(ref bx) => {
				let rt0 = bx.0.to_rust_expr(scope, idcell);
				let rt1 = bx.1.to_rust_expr(scope, idcell);
				RustExpr::Apply(Box::new((rt0, rt1)))
			},
		}
	}

	pub fn build_rust(&self) -> Vec<RustType> {
		let mut ret = Vec::new();
		let mut root = Vec::new();
		let mut idcell = 0;
		let rsexpr = self.to_rust_expr(&mut root, &mut idcell);
		rsexpr.collect(&mut ret);
		ret
	}

	pub fn push_rust(&self, s: &mut String) {
		let rts = self.build_rust();
		for rt in rts {
			s.push_str("struct L");
			let idstr = rt.id.to_string();
			s.push_str(&idstr);
			if rt.vars.len() > 0 {
				s.push('<');
				for v in rt.vars.iter() {
					s.push('X');
					s.push_str(&v.to_string());
					s.push(',');
				}
				s.pop();
				if rt.vars.len() == 1 {
					s.push_str(">(pub PhantomData<X");
					s.push_str(&rt.vars[0].to_string());
					s.push_str(">)");
				} else {
					s.push_str(">(pub PhantomData<(");
					for v in rt.vars.iter() {
						s.push('X');
						s.push_str(&v.to_string());
						s.push(',');
					}
					s.pop();
					s.push_str(")>)");
				}
			}
			s.push_str(";impl<X1");
			for v in rt.vars.iter() {
				s.push_str(",X");
				s.push_str(&v.to_string());
			}
			s.push_str("> A<X1> for L");
			s.push_str(&idstr);
			if rt.vars.len() > 0 {
				s.push('<');
				for v in rt.vars.iter() {
					s.push('X');
					s.push_str(&v.to_string());
					s.push(',');
				}
				s.pop();
				s.push('>');
			}
			if rt.wher.len() > 0 {
				s.push_str(" where ");
				for ref w in rt.wher.iter() {
					s.push_str(&w);
					s.push(',');
				}
				s.pop();
			}
			s.push_str("{type O=");
			s.push_str(&rt.expr);
			s.push_str(";}\n");
		}
	}
}

pub fn varvec(scope: FnvHashSet<usize>) -> Vec<usize> {
	let mut vave = scope.into_iter().collect::<Vec<_>>();
	vave.sort();
	vave
}

fn rscope_add(scope: &mut Vec<FnvHashSet<usize>>, mut n: usize) {
	for set in scope.iter_mut().rev() {
		if n == 0 { return }
		set.insert(n);
		n -= 1;
	}
}

fn check_pscope<'a, 'b>(scope: &'b Vec<&'a str>, s: &'b str) -> Option<usize> {
	scope.iter().cloned().rev().position(|name| name == s)
}

#[derive(Default)]
pub struct Lambda(FnvHashMap<String, Expr>);
impl Lambda {
	fn skip_ws<'a, 'b>(scope: &'a Vec<&'b str>, s: &'a mut &'b str) -> Option<Token<'b>> {
		let mut schs = s.char_indices();
		let chs = *s;
		loop {
			let islam;
			let varstart;
			loop {
				if let Some((idx, c)) = schs.next() {
					match c {
						'(' => {
							*s = &schs.as_str();
							return Some(Token::OP)
						},
						')' => {
							*s = &schs.as_str();
							return Some(Token::CP)
						},
						'\u{3bb}' | '\\' => {
							islam = true;
							varstart = idx + c.len_utf8();
							break
						},
						c if !c.is_whitespace() => {
							islam = false;
							varstart = idx;
							break
						}
						_ => (),
					}
				} else {
					*s = &schs.as_str();
					return None
				}
			}
			let varend = if let Some(varend) = chs[varstart..].find(|ch:char| ch.is_whitespace() || ch == '\u{3bb}' || ch == '(' || ch == ')' || ch == '\\') {
				varstart + varend
			} else { chs.len() };
			let var = &chs[varstart..varend];
			*s = &chs[varend..];
			if islam {
				if var.len() > 0 {
					return Some(Token::L(var))
				} else {
					return None;
				}
			} else {
				if let Some(id) = check_pscope(scope, var) {
					return Some(Token::Var(id))
				}
			}
		}
	}

	fn parse_app0<'a, 'b>(&'a self, scope: &'a mut Vec<&'b str>, s: &'a mut &'b str, p: &mut Vec<usize>, pc: &mut usize) -> Option<Expr> {
		if *pc > 0 {
			*pc -= 1;
			None
		} else {
			Lambda::skip_ws(scope, s).and_then(move|tok|
				match tok {
					Token::L(var) => {
						scope.push(var);
						self.parse_app(scope, s, p, pc).map(move|expr| Expr::L(Box::new(expr)))
					},
					Token::Var(var) => Some(Expr::Var(var)),
					Token::OP => {
						p.push(scope.len());
						self.parse_app(scope, s, p, pc)
					},
					Token::CP => {
						if let Some(sl) = p.pop() {
							*pc = scope.len() - sl;
							scope.truncate(sl);
						}
						None
					},
				}
			)
		}
	}

	fn parse_app1<'a, 'b>(&'a self, mut a0: Expr, scope: &'a mut Vec<&'b str>, mut s: &'a mut &'b str, p: &mut Vec<usize>, pc: &mut usize) -> Expr {
		while let Some(a1) = self.parse_app0(scope, s, p, pc) {
			a0 = Expr::Apply(Box::new((a0, a1)));
		}
		a0
	}

	fn parse_app<'a, 'b>(&'a self, scope: &'a mut Vec<&'b str>, s: &'a mut &'b str, p: &mut Vec<usize>, pc: &mut usize) -> Option<Expr> {
		self.parse_app0(scope, s, p, pc).map(move|a0| self.parse_app1(a0, scope, s, p, pc))
	}
}

impl Eval for Lambda {
	fn line(&mut self, s: &str) -> Option<String> {
		let mut root = Vec::new();
		let mut parens = Vec::new();
		let mut pcount = 0;
		let mut ss = s;
		Some(if let Some(expr) = self.parse_app(&mut root, &mut ss, &mut parens, &mut pcount) {
			format!("{}", expr)
		} else {
			String::new()
		})
	}
	fn set(&mut self, k: String, s: &str) -> Option<String> {
		let mut root = Vec::new();
		let mut parens = Vec::new();
		let mut pcount = 0;
		let mut ss = s;
		Some(if let Some(expr) = self.parse_app(&mut root, &mut ss, &mut parens, &mut pcount) {
			let ret = format!("{}", expr);
			self.0.insert(k, expr);
			ret
		} else {
			String::new()
		})
	}
	fn spit(&self) -> String {
		let mut ret = String::from("use lambdaski::A;");
		for (k, v) in self.0.iter() {
			ret.push_str("//");
			ret.push_str(k);
			ret.push('\n');
			v.push_rust(&mut ret);
		}
		ret
	}
}

#[cfg(test)]
mod test {
	use eval::Eval;
	use super::*;
	#[test]
	fn test_parse()
	{
		let mut x = Lambda::default();
		assert_eq!(x.line("\\x \\y \\z (x y) (z y x (\\x x y) z)").unwrap(), "\u{3bb}\u{3bb}\u{3bb}((2 1) ((((0 1) 2) \u{3bb}(0 2)) 0))");
	}
}
