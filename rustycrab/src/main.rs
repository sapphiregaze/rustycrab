use std::env;
use std::fs;

use logos::Logos;

use lexer::Token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <source_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let src = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("error reading file '{}': {}", filename, e);
            std::process::exit(1);
        }
    };

    let mut lex = Token::lexer(&src);
    while let Some(tok) = lex.next() {
        println!("{:?}", tok);
    }
}
