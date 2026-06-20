use std::env;
use std::path::Path;
use std::process::{exit, Command, ExitStatus, Stdio};

fn run_cmd(mut cmd: Command, quiet: bool) -> Option<ExitStatus> {
    if quiet {
        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());
    }
    match cmd.status() {
        Ok(status) if status.success() => Some(status),
        _ => None,
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: driver <input_file> [--quiet]");
        exit(1);
    }

    let quiet = args.iter().any(|arg| arg == "--quiet");

    let positional_args: Vec<&String> = args.iter().skip(1).filter(|arg| !arg.starts_with('-')).collect();
    if positional_args.is_empty() {
        eprintln!("Error: Missing input file path.");
        exit(1);
    }
    let input = positional_args[0];

    let base = Path::new(input)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(input);

    let mut pipeline = Some(());

    if !quiet {
        pipeline = pipeline
            .and_then(|_| {
                let mut cmd = Command::new("mill");
                cmd.arg("--bsp-install");
                run_cmd(cmd, quiet)
            })
            .map(|_| ()); // <-- Converts Option<ExitStatus> back to Option<()>

        pipeline = pipeline
            .and_then(|_| {
                let mut cmd = Command::new("scalafmt");
                cmd.arg("./src/");
                run_cmd(cmd, quiet)
            })
            .map(|_| ()); // <-- Converts Option<ExitStatus> back to Option<()>
    }

    let pipeline = pipeline
        .and_then(|_| {
            let mut cmd = Command::new("mill");
            cmd.args(["run", input]);
            run_cmd(cmd, quiet)
        })
        .and_then(|_| {
            let mut gcc = Command::new("gcc");
            let asm_file = ["build/", base, ".s"].concat();
            let out_file = ["build/", base].concat();
            
            let mut stdlib_c_files = Vec::new();

            if let Ok(entries) = std::fs::read_dir("stdlib") {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("c") {
                        if let Some(path_str) = path.to_str() {
                            stdlib_c_files.push(path_str.to_string());
                        }
                    }
                }
            }

            gcc.args(["-Wall", "-Wextra", "-Wpedantic", "-Werror", &asm_file]);
            gcc.args(&stdlib_c_files);
            gcc.args(["-o", &out_file]);

            run_cmd(gcc, quiet)
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