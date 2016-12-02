use std::cell::RefCell;
use fnv::{FnvHashMap, FnvHashSet};
use eval::Eval;

enum Token<'a> {
	Var(u32), L(&'a str), OP, CP,
}

struct RustType {
	pub vars: Vec<u32>,
	pub expr: String,
	pub wher: Vec<String>,
	pub id: u32,
}

impl RustType {
	pub fn new(idcell: &mut u32) -> RustType {
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
	Var(u32),
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
	Var(u32),
	L(Box<Expr>),
	Apply(Box<(Expr, Expr)>),
}

impl Expr {
	pub fn push_to_string(&self, s: &mut String) {
		match *self {
			Expr::Var(id) => s.push_str(&id.to_string()),
			Expr::L(ref expr) => {
				s.push('\u{3bb}');
				s.push(' ');
				expr.push_to_string(s);
			},
			Expr::Apply(ref bx) => {
				s.push('(');
				bx.0.push_to_string(s);
					s.push(' ');
				bx.1.push_to_string(s);
				s.push(')');
			},
		}
	}

	pub fn to_string(&self) -> String {
		let mut ret = String::new();
		self.push_to_string(&mut ret);
		ret
	}

	pub fn to_rust_expr<'a, 'b>(&'a self, scope: &'a RustScope<'a>, idcell: &'a mut u32) -> RustExpr {
		match *self {
			Expr::Var(x) => {
				scope.add(x);
				RustExpr::Var(x)
			},
			Expr::L(ref expr) => {
				let newscope = scope.spawn();
				let mut rt = RustType::new(idcell);
				let rte = expr.to_rust_expr(&newscope, idcell);
				rt.vars = newscope.varvec();
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
		let root = RustScope::new();
		let mut idcell = 0;
		let rsexpr = self.to_rust_expr(&root, &mut idcell);
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

trait RustScopeTrait {
	fn add<'a, 'b>(&'a self, n: u32);
}

struct RustScope<'a> {
	uplink: Option<&'a RustScopeTrait>,
	vars: RefCell<FnvHashSet<u32>>,
}

impl<'a> RustScope<'a> {
	pub fn new() -> RustScope<'static> {
		RustScope {
			uplink: None,
			vars: RefCell::default(),
		}
	}

	pub fn spawn<'b>(&'b self) -> RustScope<'b>
	{
		RustScope {
			uplink: Some(self),
			vars: RefCell::default(),
		}
	}

	pub fn varvec(&self) -> Vec<u32> {
		let mut vave = self.vars.borrow().iter().cloned().collect::<Vec<_>>();
		vave.sort();
		vave
	}
}

impl<'a> RustScopeTrait for RustScope<'a> {
	fn add(&self, n: u32) {
		if n > 0 {
			if n > 1 {
				self.vars.borrow_mut().insert(n);
			}
			if let Some(uplink) = self.uplink {
				uplink.add(n - 1);
			}
		}
	}
}

trait ParseScopeTrait {
	fn check<'a, 'b>(&'a self, &'b str, n: u32) -> u32;
}

struct ParseScope<'a> {
	uplink: Option<&'a ParseScopeTrait>,
	name: &'a str,
}

impl<'a> ParseScope<'a> {
	pub fn new() -> ParseScope<'static> {
		ParseScope {
			uplink: None,
			name: "",
		}
	}

	pub fn spawn<'b>(&'a self, k: &'b str) -> ParseScope<'b>
		where 'a: 'b
	{
		ParseScope {
			uplink: Some(self as &ParseScopeTrait),
			name: k,
		}
	}
}

impl<'a> ParseScopeTrait for ParseScope<'a> {
	fn check<'b, 'c>(&'b self, s: &'c str, n: u32) -> u32 {
		if self.name == s {
			n
		} else if let Some(uplink) = self.uplink {
			uplink.check(s, n+1)
		} else {
			0
		}
	}
}

#[derive(Default)]
pub struct Lambda(FnvHashMap<String, Expr>);
impl Lambda {
	fn skip_ws<'a, 'b, 'c>(scope: &'c ParseScope<'b>, s: &'c mut &'a str) -> Option<Token<'a>> {
		let mut schs = s.char_indices();
		let chs = schs.as_str();
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
							schs.next();
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
			let varend = varstart + chs[varstart..].find(|ch:char| ch.is_whitespace() || ch == '\u{3bb}' || ch == '(' || ch == ')' || ch == '\\').unwrap_or(chs.len());
			let var = &chs[varstart..varend];
			*s = &chs[varend..];
			if islam {
				if var.len() > 0 {
					return Some(Token::L(var))
				} else {
					return None;
				}
			} else {
				let id = scope.check(var, 1);
				if id > 0 {
					return Some(Token::Var(id))
				}
			}
		}
	}

	fn parse_app0<'a, 'b, 'c>(&'c self, scope: &'c ParseScope<'b>, s: &'c mut &'a str) -> Option<Expr> {
		Lambda::skip_ws(scope, s).and_then(move|tok|
			match tok {
				Token::L(var) =>{
					let expr = {
						let newscope = scope.spawn(&var);
						self.parse_app(&newscope, s)
					};
					expr.map(move|expr| Expr::L(Box::new(expr)))
				},
				Token::Var(var) => Some(Expr::Var(var)),
				Token::OP => self.parse_app(scope, s),
				Token::CP => None,
			}
		)
	}

	fn parse_app1<'a, 'b, 'c>(&'c self, mut a0: Expr, scope: &'c ParseScope<'b>, mut s: &'c mut &'a str) -> Expr {
		while let Some(a1) = self.parse_app0(scope, s) {
			a0 = Expr::Apply(Box::new((a0, a1)));
		}
		a0
	}

	fn parse_app<'a, 'b, 'c>(&'c self, scope: &'c ParseScope<'b>, s: &'c mut &'a str) -> Option<Expr> {
		self.parse_app0(scope, s).map(move|a0| self.parse_app1(a0, scope, s))
	}
}

impl Eval for Lambda {
	fn line(&mut self, s: &str) -> Option<String> {
		let root = ParseScope::new();
		let mut ss = s;
		Some(if let Some(expr) = self.parse_app(&root, &mut ss) {
			expr.to_string()
		} else {
			String::new()
		})
	}
	fn set(&mut self, k: String, s: &str) -> Option<String> {
		let root = ParseScope::new();
		let mut ss = s;
		Some(if let Some(expr) = self.parse_app(&root, &mut ss) {
			let ret = expr.to_string();
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
