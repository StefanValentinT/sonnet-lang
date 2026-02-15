mod ast;
mod ctfe;
mod gen_names;
mod lexer;
mod llvm_codegen;
mod lsp;
mod parser;
mod preprocess;
mod queue;
mod semantic;
mod stdlib;
mod tac;
mod utils;

use crate::ctfe::perform_ctfe_pass;
use crate::{
    lexer::lex_string, llvm_codegen::emit_llvm, parser::parse, semantic::semantic_analysis,
    tac::gen_tac,
};
use clap::Parser;
use clap::Subcommand;
use std::process::Stdio;
use std::{fs, io::Write, path::Path, process::Command};

macro_rules! vprintln {
    ($($arg:tt)*) => {
        unsafe {
            if VERBOSE {
                println!($($arg)*);
            }
        }
    }
}

static mut VERBOSE: bool = false;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Run(RunArgs),
    Lsp,
}

#[derive(Parser, Debug)]
struct RunArgs {
    #[arg(long)]
    lex: bool,

    #[arg(long)]
    parse: bool,

    #[arg(long)]
    codegen: bool,

    #[arg(long)]
    tac: bool,

    #[arg(long)]
    validate: bool,

    #[arg(short)]
    e: bool,

    #[arg(short, long)]
    verbose: bool,

    #[arg(short = 'f', long = "single-file")]
    single_file: bool,

    filename: Option<String>,
}

pub fn compile_ir(input_path: &str, asm_text: &str, object_only: bool) -> String {
    let input_path = Path::new(input_path);
    let build_dir = input_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .join("build");

    if let Err(e) = std::fs::create_dir_all(&build_dir) {
        eprintln!("Failed to create build directory '{:?}': {}", build_dir, e);
        std::process::exit(1);
    }

    let llvm_file = build_dir
        .join(input_path.file_stem().unwrap())
        .with_extension("ll");

    let file = build_dir.join(input_path.file_stem().unwrap());

    let output_file = if object_only {
        file.with_extension(".o").to_string_lossy().to_string()
    } else {
        file.with_extension("").to_string_lossy().to_string()
    };

    if let Err(e) = std::fs::write(&llvm_file, asm_text) {
        vprintln!("Failed to write LLVM IR to '{:?}': {}", llvm_file, e);
        std::process::exit(1);
    }
    vprintln!("LLVM IR written to '{:?}'", llvm_file);

    let mut cmd = Command::new("cc");
    cmd.arg(&llvm_file)
        .arg("src/runtime/runtime.c")
        .stderr(Stdio::piped());
    if object_only {
        cmd.arg("-c");
    }
    cmd.arg("-o").arg(&output_file);

    let output = cmd.output().unwrap_or_else(|e| {
        eprintln!("Failed to invoke compiler: {}", e);
        std::process::exit(1);
    });

    if !output.status.success() {
        eprintln!("Compilation failed with status: {}", output.status);
        eprintln!(
            "=== Compiler stdout ===\n{}",
            String::from_utf8_lossy(&output.stdout)
        );
        eprintln!(
            "=== Compiler stderr ===\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
        std::process::exit(1);
    }

    vprintln!("Output written to '{}'", output_file);
    output_file
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Lsp) => {
            lsp::run_language_server().await;
        }

        Some(Commands::Run(args)) => {
            run_command(args).await;
        }

        None => {
            start_repl().await;
        }
    }
}

async fn run_command(args: RunArgs) {
    unsafe {
        VERBOSE = args.verbose;
    }
    let filename = match (args.single_file, args.filename.as_ref()) {
        (true, Some(f)) => f.clone(),

        (true, None) => {
            eprintln!("Error: --single-file requires a file path");
            std::process::exit(1);
        }

        (false, Some(_)) => {
            eprintln!("Error: No file path allowed without --single-file");
            std::process::exit(1)
        }

        (false, None) => "main.hk".to_string(),
    };

    let content = fs::read_to_string(&filename).expect("Failed to read the input file");

    vprintln!("Processing file: {}", &filename);

    let cleaned = preprocess::preprocess(&content);

    let lexeme = lex_string(cleaned);
    vprintln!("Lexeme: {:?}", lexeme);
    if args.lex {
        return;
    }

    let ast = parse(lexeme);
    vprintln!("AST:\n{:#?}", ast);
    if args.parse {
        return;
    }

    let transformed_ast = semantic_analysis(ast);
    vprintln!("AST after Semantic Analysis:\n{:#?}", transformed_ast);
    if args.validate {
        return;
    }

    let ctfe_ast = match perform_ctfe_pass(transformed_ast) {
        Ok(ast) => {
            vprintln!("AST after CTFE:\n{:#?}", ast);
            ast
        }
        Err(e) => {
            eprintln!("CTFE error: {}", e);
            std::process::exit(1);
        }
    };
    vprintln!("AST after CTFE:\n{:#?}", ctfe_ast);

    let tac_ast = gen_tac(ctfe_ast);
    vprintln!("TAC-AST:\n{:#?}", tac_ast);
    if args.tac {
        return;
    }

    let llvm_ir = emit_llvm(tac_ast);
    vprintln!("LLVM IR:\n{}", llvm_ir);
    if args.codegen {
        return;
    }

    let output_file = compile_ir(&filename, &llvm_ir, false);

    if args.e {
        vprintln!("Running executable...");
        let output = std::process::Command::new(&output_file)
            .output()
            .expect("Failed to execute program");

        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();

        std::process::exit(output.status.code().unwrap_or(1));
    }
}

async fn start_repl() {
    println!(
        r#"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~                                    ~
~  _   _           _   _             ~
~ | | | |   __ _  (_) | | __  _   _  ~
~ | |_| |  / _` | | | | |/ / | | | | ~
~ |  _  | | (_| | | | |   <  | |_| | ~
~ |_| |_|  \__,_| |_| |_|\_\  \__,_| ~
~                                    ~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"#
    );
    println!("InDev Version 0.1.3 of the Haiku Programming Language Compiler");

    println!("Starting REPL...");
    std::process::exit(0);
}
