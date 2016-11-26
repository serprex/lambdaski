use eval::Eval;

#[derive(Default)]
pub struct Prolog;
impl Eval for Prolog {
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
