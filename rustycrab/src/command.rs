use std::path::PathBuf;

use clap::Arg;
use clap::ArgAction;
use clap::Command;
use clap::value_parser;

pub fn build_command() -> Command {
    Command::new("rc")
        .version(env!("CARGO_PKG_VERSION"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .arg(
            Arg::new("file_path")
                .help("Provide the file path for the input C file")
                .value_parser(value_parser!(PathBuf))
                .required(true),
        )
        .arg(
            Arg::new("verbose")
                .short('v')
                .long("verbose")
                .action(ArgAction::SetTrue)
                .help("Enable verbose output"),
        )
}
