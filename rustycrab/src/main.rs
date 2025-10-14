mod command;

use std::env;
use std::fs;
use std::path::PathBuf;
use std::process;

<<<<<<< HEAD
use command::build_command;
use lexer::Lexer;
use tracing::debug;
use tracing::error;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt;

fn main() {
    // Our parser expects empty strings, so this should parse successfully
    println!("{:?}", parser().parse("").into_result());

    // Anything other than an empty string should produce an error
    println!("{:?}", parser().parse("123"));

    let mut command = build_command();

    if env::args().len() == 1 {
        command.print_help().expect("failed to print help");
        process::exit(0);

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

    let mut lexer = Lexer::new(&src);
    for result in lexer.iter() {
        match result {
            Ok(token) => debug!("Token: {:?}", token),
            Err(e) => error!("Lexing Error: {:?}", e),
        }
    }
}
