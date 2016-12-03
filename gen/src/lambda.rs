use std::fmt::{self, Write};
use std::rc::Rc;
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

enum RustExpr {
	Var(usize),
	L(Box<(Rc<RustType>, RustExpr)>),
	Apply(Box<(RustExpr, RustExpr)>),
}

impl RustExpr {
	pub fn build_vars(&self, expr: &mut String, wher: &mut Vec<String>) {
		match *self {
			RustExpr::Var(var) => {
				write!(expr, "X{}", var).ok();
			},
			RustExpr::L(ref bx) => {
				let lrt = &bx.0;
				write!(expr, "L{}", lrt.id).ok();
				if lrt.vars.len() > 0 {
					expr.push('<');
					for v in lrt.vars.iter() {
						write!(expr, "X{},", v-1).ok();
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
				write!(expr, "<{} as A<{}>>::O", bx0, bx1).ok();
				wher.push(format!("{}:A<{}>", bx0, bx1));
			},
		}
	}
}

impl RustType {
	pub fn push_rust<'a>(&self, s: &mut String) {
		write!(s, "struct L{}", self.id).ok();
		if self.vars.len() > 0 {
			s.push('<');
			for v in self.vars.iter() {
				write!(s, "X{},", v).ok();
			}
			s.pop();
			if self.vars.len() == 1 {
				write!(s, ">(pub PhantomData<X{}>)", self.vars[0]).ok();
			} else {
				s.push_str(">(pub PhantomData<(");
				for v in self.vars.iter() {
					write!(s, "X{},", v).ok();
				}
				s.pop();
				s.push_str(")>)");
			}
		}
		s.push_str(";impl<X1");
		for v in self.vars.iter() {
			write!(s, ",X{}", v).ok();
		}
		write!(s, "> A<X1> for L{}", self.id).ok();
		if self.vars.len() > 0 {
			s.push('<');
			for v in self.vars.iter() {
				write!(s, "X{},", v).ok();
			}
			s.pop();
			s.push('>');
		}
		if self.wher.len() > 0 {
			s.push_str(" where ");
			for ref w in self.wher.iter() {
				write!(s, "{},", w).ok();
			}
			s.pop();
		}
		write!(s, "{{type O={}}};\n", self.expr).ok();
	}
}

enum Expr {
	Var(usize),
	L(Box<Expr>),
	Apply(Box<(Expr, Expr)>),
}

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Expr::Var(id) => write!(f, "{}", id),
			Expr::L(ref expr) => write!(f, "\u{3bb}{}", expr),
			Expr::Apply(ref bx) => write!(f, "({} {})", bx.0, bx.1),
		}
	}
}

impl Expr {
	pub fn to_rust_expr(&self, scope: &mut Vec<FnvHashSet<usize>>, idcell: &mut usize, typecache: &mut FnvHashMap<String, Rc<RustType>>) -> RustExpr {
		match *self {
			Expr::Var(x) => {
				rscope_add(scope, x);
				RustExpr::Var(x)
			},
			Expr::L(ref expr) => {
				scope.push(FnvHashSet::default());
				let mut rsexpr = String::new();
				let mut rswher = Vec::new();
				let rte = expr.to_rust_expr(scope, idcell, typecache);
				rte.build_vars(&mut rsexpr, &mut rswher);
				if let Some(rt) = typecache.get(&rsexpr) {
					return RustExpr::L(Box::new((rt.clone(), rte)));
				}
				let key = rsexpr.clone();
				let rt = Rc::new(RustType {
					vars: varvec(scope.pop().unwrap()),
					expr: rsexpr,
					wher: rswher,
					id: *idcell,
				});
				*idcell += 1;
				typecache.insert(key, rt.clone());
				RustExpr::L(Box::new((rt, rte)))
			},
			Expr::Apply(ref bx) => {
				let rt0 = bx.0.to_rust_expr(scope, idcell, typecache);
				let rt1 = bx.1.to_rust_expr(scope, idcell, typecache);
				RustExpr::Apply(Box::new((rt0, rt1)))
			},
		}
	}

	pub fn build_rust(&self, typecache: &mut FnvHashMap<String, Rc<RustType>>) -> RustExpr {
		let mut root = Vec::new();
		self.to_rust_expr(&mut root, &mut 0, typecache)
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
					return None
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
		let mut types = FnvHashMap::default();
		for (k, v) in self.0.iter() {
			let rsexpr = v.build_rust(&mut types);
			ret.push_str("pub type ");
			ret.push_str(k);
			ret.push('=');
			let mut wher = Vec::new();
			let mut expr = String::new();
			rsexpr.build_vars(&mut expr, &mut wher);
			ret.push_str(&expr);
			ret.push(';');
		}
		for v in types.values() {
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
