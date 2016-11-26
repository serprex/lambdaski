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

fn runstdin<T: Eval>(mut eval: T) {
	let sin = io::stdin();
	for line in sin.lock().lines() {
		if line.ok().map(|s| eval.line(&s)).is_none() {
			break
		}
	}
}

fn interact<T: Eval>(mut eval: T) {
	let mut rl = Editor::<()>::new();
	while let Ok(line) = rl.readline("> ") {
		if line.starts_with(":save ") {
			match File::create(&line[6..]) {
				Ok(mut f) => {f.write(eval.spit().as_bytes()).ok();},
				Err(e) => println!("{}", e),
			}
		}
		else if line.starts_with(":set ") {
			if let Some(identstr) = line.split(' ').nth(1) {
				let ident = String::from(identstr);
				let startidx = 6+ident.len();
				if let Some(result) = eval.set(ident, &line[startidx..]) {
					if result != "" {
						println!("{}", result);
					}
				} else {
					break
				}

			}
		}
		else if let Some(result) = eval.line(&line) {
			if result != "" {
				println!("{}", result);
			}
		} else {
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
		Some(ref a0) if a0 == "lambda" => repl(args.nth(2), lambda::Lambda::default()),
		Some(ref a0) if a0 == "prolog" => repl(args.nth(2), prolog::Prolog::default()), 
		Some(ref a0) if a0 == "ski" => repl(args.nth(2), ski::Ski::default()),
		_ => println!("lambdagen [lambda|ski|prolog] [stdin]"),
	}
}
