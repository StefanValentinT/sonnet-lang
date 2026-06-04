use std::env;
use std::path::Path;
use std::process::{Command, ExitStatus};

fn run_cmd(mut cmd: Command) -> Option<ExitStatus> {
	match cmd.status() {
		Ok(status) if status.success() => Some(status),
		_ => None,
	}
}

fn main() {
	let args: Vec<String> = env::args().collect();
	if args.len() < 2 { return; }
	let input = &args[1];
	let base = Path::new(input).file_stem().and_then(|s| s.to_str()).unwrap_or(input);
	let pipeline = Some(())
		.and_then(|_| {
			let mut cmd = Command::new("mill");
			cmd.arg("--bsp-install");
			run_cmd(cmd)
		})
		.and_then(|_| {
			let mut cmd = Command::new("scalafmt");
			cmd.arg("./src/");
			run_cmd(cmd)
		})
		.and_then(|_| {
			let mut cmd = Command::new("mill");
			cmd.args(["run", input]);
			run_cmd(cmd)
		})
		.and_then(|_| {
			let mut gcc = Command::new("gcc");
			gcc.args([&["build/", base, ".s"].concat(), "runtime.c", "-o", &["build/", base].concat()]);
			run_cmd(gcc)
		})
		.and_then(|_| {
			let mut run = Command::new(&["./build/", base].concat());
			match run.status() {
				Ok(status) => {
					if let Some(code) = status.code() {
						println!("{}", code);
					}
					Some(status)
				}
				_ => None,
			}
		});
	if pipeline.is_none() {
		eprintln!("Compiler driver aborted!");
	}
}