mod ast;
mod lexer;
mod parser;
//mod typer;

use crate::lexer::tokenize;
use crate::parser::parse;
//use crate::typer::typecheck;

use std::env;
use std::fs;
use std::io::{self, Write};

fn run_source(source: &str) {
    println!("Source:\n{}", source);

    let tokens = tokenize(source);
    println!("\nTokens: {:?}", tokens);

    let ast = parse(tokens);
    println!("Ast:\n{:#?}", ast);

    //let typed_ast = typecheck(ast);
    //println!("Typed Ast:\n{:#?}", typed_ast);
}

fn repl() {
    println!("Haiku REPL (type 'exit' to quit)");

    let mut input = String::new();

    loop {
        input.clear();
        print!("> ");
        io::stdout().flush().unwrap();

        if io::stdin().read_line(&mut input).is_err() {
            println!("Error reading input");
            continue;
        }

        let line = input.trim();

        if line == "exit" {
            break;
        }

        if line.is_empty() {
            continue;
        }

        run_source(line);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        repl();
        return;
    }

    let filename = &args[1];

    let content = match fs::read_to_string(filename) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            std::process::exit(1);
        }
    };

    run_source(&content);
}