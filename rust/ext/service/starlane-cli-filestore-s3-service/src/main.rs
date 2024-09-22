use clap::Parser;
use starlane::dialect::cli::filestore::Cli;
use starlane::dialect::cli::filestore::Commands;
use std::env::VarError;
use std::io::{Read, Write};
use std::path::StripPrefixError;
use std::process::{ExitCode, Termination};
use std::io;
use thiserror::Error;


pub const FILE_STORE_ROOT: &'static str = "FILE_STORE_ROOT";



fn main() -> ExitCode{
    match run() {
        Ok(_) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{}",err.to_string());
            ExitCode::FAILURE
        }
    }
}


fn run() -> Result<(),Error> {


    let cli = Cli::parse();
    if let Commands::Init = cli.command {
        todo!()
    }


    match cli.command {
        Commands::Init => {
            todo!()
        }
        Commands::Write { path } => {
            todo!()

        }
        Commands::Read { path } => {
            todo!()
        }
        Commands::Mkdir { path } => {
            todo!()
        }
        Commands::Delete { path } => {
            todo!()
        }
        Commands::List { path: Option::Some(path)} => {
            todo!()
        }
        Commands::List { path: Option::None } => {
            todo!()
        }
        Commands::Pwd =>  {
            todo!()
        }

        Commands::Exists { path } => {
            todo!()
        }
    }
}



#[derive(Error, Debug)]
pub enum Error{
    #[error("could not access local filesystem")]
    FileSys(#[from] io::Error),
    #[error("{0}")]
    String( String),
    #[error("{0}")]
    Path(#[from] StripPrefixError),
    #[error("{0}")]
    VarError(#[from] VarError),

}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}


impl From<&str> for Error {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}



#[cfg(test)]
pub mod test {
    #[test]
    pub fn test() {


    }

}