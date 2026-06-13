use std::env;
use std::path::Path;
use std::process::{Command, ExitStatus, exit};

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
			let asm_file = ["build/", base, ".s"].concat();
			let out_file = ["build/", base].concat();
			gcc.args([
				"-Wall",
				"-Wextra",
				"-Wpedantic",
				"-Werror",
				&asm_file,
				"runtime.c",
				"-o",
				&out_file,
			]);
			run_cmd(gcc)
		})
		.and_then(|_| {
			let mut run = Command::new(&["./build/", base].concat());
			run.status().ok()
		});
	match pipeline {
		Some(status) => {
			exit(status.code().unwrap_or(1));
		}
		None => {
			eprintln!("Compiler driver aborted!");
			exit(1);
		}
	}
}