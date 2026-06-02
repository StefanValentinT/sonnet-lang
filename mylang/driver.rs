
use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
	let args: Vec<String> = env::args().collect();
	if args.len() < 2 { return; }
	let input = &args[1];
	let base = Path::new(input).file_stem().and_then(|s| s.to_str()).unwrap_or(input);

	let _ = Command::new("mill").arg("--bsp-install").status();
	let _ = Command::new("scalafmt").arg("./src/").status();
	let _ = Command::new("mill").args(["run", input]).status();
	let _ = Command::new("gcc").args([&["build/", base, ".s"].concat(), "runtime.c", "-o", &["build/", base].concat()]).status();
	
	if let Ok(status) = Command::new(&["./build/", base].concat()).status() {
		if let Some(code) = status.code() {
			println!("{}", code);
		}
	}
}