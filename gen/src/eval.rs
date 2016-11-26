pub trait Eval {
	fn line(&mut self, &str) -> Option<String>;
	fn set(&mut self, String, &str) -> Option<String>;
	fn spit(&self) -> String;
}
