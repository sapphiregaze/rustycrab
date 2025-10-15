mod command;

use std::env;
use std::fs;
use std::path::PathBuf;
use std::process;

use command::build_command;
use lexer::Token;
use logos::Logos;
use tracing::debug;
use tracing::error;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt;

fn main() {
    let mut command = build_command();

    if env::args().len() == 1 {
        command.print_help().expect("failed to print help");
        process::exit(0);
    }

    let matches = command.get_matches();

    let verbose = matches.get_flag("verbose");
    let log_level = if verbose { "debug" } else { "warn" };
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::new(log_level))
        .event_format(fmt::format().without_time().compact())
        .init();

    let file_path = matches.get_one::<PathBuf>("file_path").unwrap();
    let src = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            error!(r#"Error reading file "{}": {}"#, file_path.display(), e);
            std::process::exit(1);
        }
    };

    let lex = Token::lexer(&src);
    for tok_res in lex {
        let res = match tok_res {
            Ok(Token::Err(e)) => Err(e),
            Ok(t) => Ok(t),
            Err(e) => Err(e),
        };

        match res {
            Ok(token) => debug!("Token: {:?}", token),
            Err(err) => error!("Lexing error: {:?}", err),
        }
    }
}
