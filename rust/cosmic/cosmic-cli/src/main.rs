#![allow(warnings)]

#[macro_use]
extern crate clap;

#[macro_use]
extern crate text_io;

use clap::arg;
use clap::command;
use clap::{App, Arg, Args, Command as ClapCommand, Parser, Subcommand};
use cosmic_hyperlane::test_util::SingleInterchangePlatform;
use cosmic_hyperlane::HyperwayEndpointFactory;
use cosmic_hyperlane_tcp::HyperlaneTcpClient;
use cosmic_hyperspace::driver::control::{ControlCliSession, ControlClient};
use cosmic_nom::new_span;
use cosmic_space::command::{CmdTransfer, Command, RawCommand};
use cosmic_space::err::SpaceErr;
use cosmic_space::hyper::{InterchangeKind, Knock};
use cosmic_space::point::{Point, ToSurface};
use cosmic_space::log::RootLogger;
use cosmic_space::parse::error::result;
use cosmic_space::parse::{command_line, upload_blocks};
use cosmic_space::substance::Substance;
use cosmic_space::util::{log, ToResolved};
use cosmic_space::wave::core::ReflectedCore;
use std::fs::File;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use std::{
    io::{Cursor, Read, Seek, Write},
    path::Path,
};
use walkdir::{DirEntry, WalkDir};
use zip::{result::ZipError, write::FileOptions};

#[tokio::main]
async fn main() -> Result<(), SpaceErr> {
    let home_dir: String = match dirs::home_dir() {
        None => ".".to_string(),
        Some(dir) => dir.display().to_string(),
    };
    let matches = ClapCommand::new("cosmic-cli")
        .arg(
            Arg::new("host")
                .short('h')
                .long("host")
                .takes_value(true)
                .value_name("host")
                .required(false)
                .default_value("localhost"),
        )
        .arg(
            Arg::new("certs")
                .short('c')
                .long("certs")
                .takes_value(true)
                .value_name("certs")
                .required(false)
                .default_value(format!("{}/.starlane/localhost/certs", home_dir).as_str()),
        )
        .subcommand(ClapCommand::new("script"))
        .allow_external_subcommands(true)
        .get_matches();

    let host = matches.get_one::<String>("host").unwrap().clone();
    let certs = matches.get_one::<String>("certs").unwrap().clone();
    let session = Session::new(host, certs).await?;

    if matches.subcommand_name().is_some() {
        session.command(matches.subcommand_name().unwrap()).await
    } else {
        loop {
            let line: String = text_io::try_read!("{};").map_err(|e| SpaceErr::new(500, "err"))?;

            let line_str = line.trim();

            if "exit" == line_str {
                return Ok(());
            }
            println!("> {}", line_str);
            session.command(line.as_str()).await?;
        }
        Ok(())
    }
}

pub struct Session {
    pub client: ControlClient,
    pub cli: ControlCliSession,
}

impl Session {
    pub async fn new(host: String, certs: String) -> Result<Self, SpaceErr> {
        let logger = RootLogger::default();
        let logger = logger.point(Point::from_str("cosmic-cli")?);
        let tcp_client: Box<dyn HyperwayEndpointFactory> = Box::new(HyperlaneTcpClient::new(
            format!("{}:{}", host, 4343),
            certs,
            Knock::default(),
            false,
            logger,
        ));

        let client = ControlClient::new(tcp_client)?;
        client.wait_for_ready(Duration::from_secs(10)).await?;
        client.wait_for_greet().await?;

        let cli = client.new_cli_session().await?;

        Ok(Self { client, cli })
    }

    async fn command(&self, command: &str) -> Result<(), SpaceErr> {
        let blocks = result(upload_blocks(new_span(command)))?;
        let mut command = RawCommand::new(command.to_string());
        for block in blocks {
            
            let path = block.name.clone();
            let metadata = std::fs::metadata(&path)?;

            let content = if metadata.is_dir() {
                let file = Cursor::new(Vec::new());

                let walkdir = WalkDir::new(&path);
                let it = walkdir.into_iter();

                let data = match zip_dir(
                    &mut it.filter_map(|e| e.ok()),
                    &path,
                    file,
                    zip::CompressionMethod::Deflated,
                ) {
                    Ok(data) => data,
                    Err(e) => return Err(SpaceErr::new(500, e.to_string())),
                };

                // return the inner buffer from the cursor
                let data = data.into_inner();
                data
            } else {
                std::fs::read(block.name.as_str())?
            };

            command
                .transfers
                .push(CmdTransfer::new(block.name, Arc::new(content)));
        }

        let core = self.cli.raw(command).await?;
        self.core_out(core);

        Ok(())
    }

    pub fn core_out(&self, core: ReflectedCore) {
        match core.is_ok() {
            true => self.out(core.body),
            false => {
                if core.body != Substance::Empty {
                    self.out(core.body);
                } else {
                    self.out_err(core.ok_or().unwrap_err());
                    std::process::exit(1);
                }
            }
        }
    }

    pub fn out(&self, substance: Substance) {
        match substance {
            Substance::Empty => {
                println!("Ok");
            }
            Substance::Err(err) => {
                println!("{}", err.to_string());
            }
            Substance::List(list) => {
                for i in list.list {
                    self.out(*i);
                }
            }
            Substance::Point(point) => {
                println!("{}", point.to_string());
            }
            Substance::Surface(surface) => {
                println!("{}", surface.to_string());
            }
            Substance::Text(text) => {
                println!("{}", text);
            }
            Substance::Stub(stub) => {
                println!("{}<{}>", stub.point.to_string(), stub.kind.to_string())
            }
            Substance::Details(details) => {
                println!(
                    "{}<{}>",
                    details.stub.point.to_string(),
                    details.stub.kind.to_string()
                )
            }
            what => {
                eprintln!(
                    "cosmic-cli not sure how to output {}",
                    what.kind().to_string()
                )
            }
        }
    }

    pub fn out_err(&self, err: SpaceErr) {
        eprintln!("{}", err.to_string())
    }
}

fn zip_dir<T>(
    it: impl Iterator<Item = DirEntry>,
    prefix: &str,
    writer: T,
    method: zip::CompressionMethod,
) -> zip::result::ZipResult<T>
where
    T: Write + Seek,
{
    let mut zip = zip::ZipWriter::new(writer);
    let options = FileOptions::default()
        .compression_method(method)
        .unix_permissions(0o755);

    let mut buffer = Vec::new();
    for entry in it {
        let path = entry.path();
        let name = path.strip_prefix(Path::new(prefix)).unwrap();

        // Write file or directory explicitly
        // Some unzip tools unzip files with directory paths correctly, some do not!
        if path.is_file() {
            zip.start_file(name.to_str().unwrap(), options)?;
            let mut f = File::open(path)?;

            f.read_to_end(&mut buffer)?;
            zip.write_all(&*buffer)?;
            buffer.clear();
        } else if !name.as_os_str().is_empty() {
            // Only if not root! Avoids path spec / warning
            // and mapname conversion failed error on unzip
            zip.add_directory(name.to_str().unwrap(), options)?;
        }
    }
    let result = zip.finish()?;
    Result::Ok(result)
}
