extern crate fnv;
extern crate rustyline;

mod eval;
mod lambda;
mod prolog;
mod ski;

use std::env;
use std::fs::File;
use std::io::{self, BufRead, Write};
use rustyline::Editor;
use eval::Eval;

fn replcore<T: Eval>(eval: &mut T, quiet: bool, line: &String) -> bool {
	if line.starts_with(":save ") {
		match File::create(&line[6..]) {
			Ok(mut f) => {f.write(eval.spit().as_bytes()).ok();},
			Err(e) => if !quiet { println!("{}", e) },
		}
	}
	else if line == ":spit" {
		println!("{}", eval.spit());
	}
	else if line.starts_with(":set ") {
		if let Some(identstr) = line.split(' ').nth(1) {
			let ident = String::from(identstr);
			let startidx = 6+ident.len();
			if let Some(result) = eval.set(ident, &line[startidx..]) {
				if !quiet && result != "" {
					println!("{}", result);
				}
			} else {
				return false
			}
		}
	}
	else if let Some(result) = eval.line(line) {
		if !quiet && result != "" {
			println!("{}", result);
		}
	} else {
		return false
	}
	true
}

fn runstdin<T: Eval>(mut eval: T) {
	let sin = io::stdin();
	for line in sin.lock().lines() {
		if let Ok(line) = line {
			if replcore(&mut eval, true, &line) {
				continue
			}
		}
		break
	}
}

fn interact<T: Eval>(mut eval: T) {
	let mut rl = Editor::<()>::new();
	while let Ok(line) = rl.readline("> ") {
		if !replcore(&mut eval, false, &line) {
			break
		}
		rl.add_history_entry(&line);
	}
}

fn repl<T: Eval>(a1: Option<String>, eval: T) {
	match a1 {
		Some(ref a1) if a1 == "stdin" => runstdin(eval),
		_ => interact(eval),
	}
}

fn main() {
	let mut args = env::args();
	match args.nth(1) {
		Some(ref a) if a == "lambda" => repl(args.nth(2), lambda::Lambda::default()),
		Some(ref a) if a == "prolog" => repl(args.nth(2), prolog::Prolog::default()), 
		Some(ref a) if a == "ski" => repl(args.nth(2), ski::Ski::default()),
		_ => println!("lambdagen [lambda|ski|prolog] [stdin]"),
	}
}
