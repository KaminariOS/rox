extern crate core;

use crate::expr::Stmt;
use ansi_rgb::{green, Foreground};
use clap::{arg, command};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;
use std::fs;

mod ast_printer;
mod expr;
mod scanner;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::resolver::Resolver;

mod environment;
mod function;
mod interpreter;
mod parser;
mod resolver;
mod types;

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
    let mut interpreter = Interpreter::new();
    if let Err(e) = run(&contents, &mut interpreter, false) {
        eprintln!("{}", e);
    };
}

fn run<'a>(line: &str, interpreter: &'a mut Interpreter, repl: bool) -> Result<(), Box<dyn Error>> {
    let mut scanner = scanner::Scanner::new(line);
    scanner.scan_tokens()?;
    let statements = {
        let mut parser = parser::Parser::new(scanner.tokens, repl, &mut interpreter.id);
        parser.parse()?
    };
    // let block = Stmt::Block { statements };
    Resolver::new(interpreter).resolve_statements(&statements)?;
    interpreter.interpret_stmts(&statements)?;
    Ok(())
    // println!("{}", print_ast(expr))
}

fn run_prompt() {
    let mut rl = Editor::<()>::new();
    let history_path = "history.txt";
    if rl.load_history(history_path).is_err() {
        println!("No previous history.");
    }
    let mut interpreter = Interpreter::new();
    loop {
        let read_line = rl.readline(&">> ".fg(green()).to_string());
        match read_line {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if let Err(e) = run(&line, &mut interpreter, true) {
                    eprintln!("{}", e);
                };
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
