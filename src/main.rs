extern crate core;

use ansi_rgb::{green, Foreground};
use clap::{arg, command};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs;

mod ast_printer;
mod expr;
mod scanner;
use crate::interpreter::{Interpreter, RuntimeError};
use ast_printer::print_ast;

mod interpreter;
mod parser;

fn main() {
    let matches = command!()
        .arg(arg!([name] "Optional file name"))
        .get_matches();
    if let Some(filename) = matches.value_of("name") {
        run_file(filename);
    } else {
        run_prompt();
    }
}

fn run_file(filename: &str) {
    println!("Running {}", filename);
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    run(&contents);
}

fn run(line: &str) -> Result<(), RuntimeError> {
    let mut scanner = scanner::Scanner::new(line);
    scanner.scan_tokens();
    let mut parser = parser::Parser::new(scanner.tokens);
    let statements = parser.parse();
    if let Err(e) = Interpreter::interpret_stmt(&statements) {
        println!("{}", e);
        Err(e)
    } else {
        Ok(())
    }
    // println!("{}", print_ast(expr))
}

fn run_prompt() {
    let mut rl = Editor::<()>::new();
    let history_path = "history.txt";
    if rl.load_history(history_path).is_err() {
        println!("No previous history.");
    }
    loop {
        let read_line = rl.readline(&">> ".fg(green()).to_string());
        match read_line {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                run(&line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
