use std::collections::HashMap;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fs::{DirBuilder, File};
use std::io::{Read, Write};
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{mpsc, Arc};
use std::{fs, thread};

use notify::{raw_watcher, Op, RawEvent, RecursiveMode, Watcher};
use tokio::fs::ReadDir;
use tokio::io::AsyncReadExt;
use tokio::sync::mpsc::Sender;
use tokio::time::Duration;
use walkdir::WalkDir;

use mesh_portal::version::latest::path::Path;

use crate::error::Error;
use crate::util;

pub enum FileCommand {
    List {
        path: Path,
        tx: tokio::sync::oneshot::Sender<Result<Vec<Path>, Error>>,
    },
    Read {
        path: Path,
        tx: tokio::sync::oneshot::Sender<Result<Arc<Vec<u8>>, Error>>,
    },
    Write {
        path: Path,
        data: Arc<Vec<u8>>,
        tx: tokio::sync::oneshot::Sender<Result<(), Error>>,
    },
    //    WriteStream{ path: Path, stream: Box<dyn AsyncRead>, tx: tokio::sync::oneshot::Sender<Result<(),Error>> },
    MkDir {
        path: Path,
        tx: tokio::sync::oneshot::Sender<Result<(), Error>>,
    },
    Watch {
        tx: tokio::sync::oneshot::Sender<Result<tokio::sync::mpsc::Receiver<FileEvent>, Error>>,
    },
    Walk {
        tx: tokio::sync::oneshot::Sender<Result<tokio::sync::mpsc::Receiver<FileEvent>, Error>>,
    },
    UnZip {
        source: String,
        target: String,
        tx: tokio::sync::oneshot::Sender<Result<(), Error>>,
    },
    RemoveDir {
        path: Path,
        tx: tokio::sync::oneshot::Sender<Result<(), Error>>,
    },
    Shutdown,
    Exists {
        path: Path,
        tx: tokio::sync::oneshot::Sender<Result<bool, Error>>,
    },
}

#[derive(Clone)]
pub struct FileAccess {
    path: String,
    tx: tokio::sync::mpsc::Sender<FileCommand>,
}

impl FileAccess {
    pub fn path(&self) -> String {
        self.path.clone()
    }

    pub fn new(path: String) -> Result<Self, Error> {
        let tx = match LocalFileAccess::new(path.clone()) {
            Ok(tx) => tx,
            Err(err) => {
                error!("could not create base_dir: {}", path);
                return Err(err);
            }
        };
        let path = fs::canonicalize(&path)?
            .to_str()
            .ok_or("turning path to string")?
            .to_string();
        Ok(FileAccess { path: path, tx: tx })
    }

    pub async fn remove_dir(&self, path: &Path) -> Result<(), Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx
            .send(FileCommand::RemoveDir {
                path: path.clone(),
                tx: tx,
            })
            .await?;
        Ok(util::wait_for_it(rx).await?)
    }

    pub async fn list(&self, path: &Path) -> Result<Vec<Path>, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx
            .send(FileCommand::List {
                path: path.clone(),
                tx: tx,
            })
            .await?;
        Ok(util::wait_for_it(rx).await?)
    }

    pub async fn read(&self, path: &Path) -> Result<Arc<Vec<u8>>, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx
            .send(FileCommand::Read {
                path: path.clone(),
                tx: tx,
            })
            .await?;
        Ok(util::wait_for_it(rx).await?)
    }

    pub async fn write(&self, path: &Path, data: Arc<Vec<u8>>) -> Result<(), Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx
            .send(FileCommand::Write {
                path: path.clone(),
                data,
                tx,
            })
            .await?;
        Ok(util::wait_for_it(rx).await?)
    }

    /*
    pub async fn write_stream( &mut self, path: &Path, stream: Box<dyn AsyncReadExt> )->Result<(),Error> {
        let (tx,mut rx) = tokio::sync::oneshot::channel();
        self.tx.send( FileCommand::WriteStream{path:path.clone(),stream,tx}).await?;
        Ok(util::wait_for_it_for(rx, Duration::from_secs(60*15)).await?)
    }

     */

    pub fn with_path(&self, path: String) -> Result<FileAccess, Error> {
        let path = format!("{}/{}", self.path, path);
        Ok(FileAccess::new(path)?)
    }

    pub async fn unzip(&self, source: String, target: String) -> Result<(), Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx
            .send(FileCommand::UnZip { source, target, tx })
            .await?;
        Ok(util::wait_for_it_for(rx, Duration::from_secs(60 * 2)).await?)
    }

    pub async fn mkdir(&self, path: &Path) -> Result<FileAccess, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx
            .send(FileCommand::MkDir {
                path: path.clone(),
                tx,
            })
            .await?;
        util::wait_for_it(rx).await?;
        self.with_path(path.to_relative())
    }

    pub async fn watch(&self) -> Result<tokio::sync::mpsc::Receiver<FileEvent>, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx.send(FileCommand::Watch { tx }).await?;
        Ok(util::wait_for_it(rx).await?)
    }

    pub async fn walk(&self) -> Result<tokio::sync::mpsc::Receiver<FileEvent>, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx.send(FileCommand::Walk { tx }).await?;
        Ok(util::wait_for_it(rx).await?)
    }

    pub async fn exists(&self, path: &Path) -> Result<bool, Error> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        self.tx
            .send(FileCommand::Exists {
                path: path.clone(),
                tx,
            })
            .await?;
        Ok(util::wait_for_it(rx).await?)
    }

    pub fn close(&self) {
        self.tx.try_send(FileCommand::Shutdown);
    }
}

#[derive(Debug, Clone)]
pub enum FileEventKind {
    Discovered,
    Create,
    Update,
    Delete,
}

#[derive(Debug, Clone)]
pub struct FileEvent {
    pub path: String,
    pub event_kind: FileEventKind,
    pub file_kind: FileSubKind,
}

#[derive(Clone)]
pub struct MemoryFileAccess {
    map: HashMap<Path, Arc<Vec<u8>>>,
}

impl MemoryFileAccess {
    pub fn new() -> Self {
        MemoryFileAccess {
            map: HashMap::new(),
        }
    }
}

pub struct LocalFileAccess {
    base_dir: String,
    rx: tokio::sync::mpsc::Receiver<FileCommand>,
}

impl LocalFileAccess {
    pub fn new(base_dir: String) -> Result<tokio::sync::mpsc::Sender<FileCommand>, Error> {
        let mut builder = DirBuilder::new();
        builder.recursive(true);
        builder.create(base_dir.clone())?;

        let (tx, rx) = tokio::sync::mpsc::channel(128);

        tokio::spawn(async move {
            Self {
                base_dir: base_dir,
                rx: rx,
            }
            .run()
            .await;
        });

        Ok(tx)
    }

    async fn run(mut self) {
        tokio::spawn(async move {
            while let Option::Some(command) = self.rx.recv().await {
                if let FileCommand::Shutdown = command {
                    break;
                }

                match self.process(command).await {
                    Ok(_) => {}
                    Err(error) => {
                        eprintln!("Error in LocalFileAccess: {}", error)
                    }
                }
            }
        });
    }

    async fn process(&mut self, command: FileCommand) -> Result<(), Error> {
        match command {
            FileCommand::List { path, tx } => {
                tx.send(self.list(path).await).unwrap_or_default();
            }
            FileCommand::Read { path, tx } => {
                tx.send(self.read(&path)).unwrap_or_default();
            }
            FileCommand::Write { path, data, tx } => {
                tx.send(self.write(&path, data)).unwrap_or_default();
            }
            /*            FileCommand::WriteStream { path: path, stream, tx } => {
                tx.send(self.write_sream(&path,stream).await);
            }*/
            FileCommand::MkDir { path, tx } => {
                tx.send(self.mkdir(&path)).unwrap_or_default();
            }
            FileCommand::Watch { tx } => {
                tx.send(self.watch()).unwrap_or_default();
            }
            FileCommand::Walk { tx } => {
                tx.send(self.walk()).unwrap_or_default();
            }
            FileCommand::UnZip { source, target, tx } => {
                tx.send(self.unzip(source, target)).unwrap_or_default();
            }
            FileCommand::Shutdown => {
                // do nothing
            }
            FileCommand::Exists { path, tx } => {
                tx.send(self.exists(&path));
            }
            FileCommand::RemoveDir { path, tx } => {
                tx.send(self.remove_dir(&path));
            }
        }
        Ok(())
    }

    async fn list(&self, dir_path: Path) -> Result<Vec<Path>, Error> {
        let path = self.cat_path(dir_path.to_relative().as_str())?;
        let mut read_dir = tokio::fs::read_dir(path).await?;

        let mut rtn = vec![];
        while let Result::Ok(Option::Some(entry)) = read_dir.next_entry().await {
            let entry = Path::make_absolute(
                entry
                    .file_name()
                    .to_str()
                    .ok_or("expected os str to be able to change to str")?,
            )?;
            let entry = dir_path.cat(&entry)?;
            rtn.push(entry);
        }
        Ok(rtn)
    }

    pub fn cat_path(&self, path: &str) -> Result<String, Error> {
        let mut path_str = path.to_string();
        if path_str.starts_with("/") {
            path_str.remove(0);
        }
        let mut path_buf = PathBuf::new();
        path_buf.push(self.base_dir.clone());
        path_buf.push(path_str);
        let path = path_buf.as_path().clone();
        let path = path.to_str().ok_or("path error")?.to_string();

        Ok(path)
    }

    pub fn exists(&self, path: &Path) -> Result<bool, Error> {
        let path = self.cat_path(path.to_relative().as_str())?;
        Ok(match File::open(&path) {
            Ok(_) => true,
            Err(_) => false,
        })
    }
}

impl LocalFileAccess {
    pub fn unzip(&mut self, source: String, target: String) -> Result<(), Error> {
        let source = format!("{}/{}", self.base_dir, source);
        let source = File::open(source)?;
        let mut archive = zip::ZipArchive::new(source)?;

        for i in 0..archive.len() {
            let mut zip_file = archive.by_index(i)?;
            if zip_file.is_dir() {
                let path = Path::from_str(format!("/{}/{}", target, zip_file.name()).as_str())?;
                self.mkdir(&path)?;
            } else {
                let path = Path::from_str(
                    format!("{}/{}/{}", self.base_dir, target, zip_file.name()).as_str(),
                )?;
                let parent =
                    Path::from_str(format!("/{}/{}", target, zip_file.name()).as_str())?.parent();
                match parent {
                    None => {}
                    Some(parent) => {
                        self.mkdir(&parent)?;
                    }
                }

                let mut file = fs::File::create(path.to_string().clone())?;
                std::io::copy(&mut zip_file, &mut file)?;
            }
        }

        Ok(())
    }

    pub fn read(&self, path: &Path) -> Result<Arc<Vec<u8>>, Error> {
        let path = self.cat_path(path.to_relative().as_str())?;
        let mut buf = vec![];
        let mut file = match File::open(&path) {
            Ok(file) => file,
            Err(error) => {
                return Result::Err(
                    format!("{} PATH: {}", error.to_string(), path.to_string()).into(),
                );
            }
        };
        file.read_to_end(&mut buf)?;
        Ok(Arc::new(buf))
    }

    pub fn write(&mut self, path: &Path, data: Arc<Vec<u8>>) -> Result<(), Error> {
        if let Option::Some(parent) = path.parent() {
            self.mkdir(&parent)?;
        }

        let path = self.cat_path(path.to_string().as_str())?;
        let mut file = File::create(&path)?;
        file.write(data.as_slice()).unwrap();

        Ok(())
    }

    /*
    pub async fn write_stream(&mut self, path: &Path, mut stream: Box<dyn AsyncReadExt>) -> Result<(), Error> {

        if let Option::Some(parent) = path.parent(){
            self.mkdir(&parent)?;
        }

        let path = self.cat_path(path.to_relative().as_str() )?;
        let mut file = tokio::fs::File::create(&path).await?;

        tokio::io::copy(&mut stream,&mut file ).await?;

        Ok(())
    }
     */

    fn mkdir(&mut self, path: &Path) -> Result<(), Error> {
        let path = self.cat_path(path.to_relative().as_str())?;
        let mut builder = DirBuilder::new();
        builder.recursive(true);
        builder.create(path.clone())?;
        Ok(())
    }

    fn remove_dir(&mut self, path: &Path) -> Result<(), Error> {
        let path = self.cat_path(path.to_relative().as_str())?;
        fs::remove_dir_all(path)?;
        Ok(())
    }

    fn walk(&mut self) -> Result<tokio::sync::mpsc::Receiver<FileEvent>, Error> {
        let (event_tx, event_rx) = tokio::sync::mpsc::channel(128);
        let tokio_runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;
        let base_dir = self.base_dir.clone();

        thread::spawn(move || {
            for entry in WalkDir::new(base_dir.clone()) {
                match entry {
                    Ok(dir_entry) => match dir_entry.path().to_str() {
                        Some(path) => {
                            let event = FileEvent {
                                path: path.to_string(),
                                event_kind: FileEventKind::Discovered,
                                file_kind: FileSubKind::Dir,
                            };
                            let event_tx = event_tx.clone();
                            tokio_runtime.block_on(async move {
                                event_tx.send(event).await;
                            })
                        }
                        None => {
                            return;
                        }
                    },
                    Err(error) => {
                        eprintln!("Error when walking filesystem: {}", error);
                    }
                }
            }
        });

        Ok(event_rx)
    }

    fn watch(&mut self) -> Result<tokio::sync::mpsc::Receiver<FileEvent>, Error> {
        let (tx, rx) = mpsc::channel();
        let (event_tx, event_rx) = tokio::sync::mpsc::channel(128);

        let tokio_runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;
        let base_dir = self.base_dir.clone();

        thread::spawn(move || {
            let mut watcher = raw_watcher(tx).unwrap();
            watcher
                .watch(base_dir.clone(), RecursiveMode::Recursive)
                .unwrap();

            loop {
                match rx.recv() {
                    Ok(RawEvent {
                        path: Some(path),
                        op: Ok(op),
                        cookie: _,
                    }) => {
                        let event_kind = match op {
                            Op::CREATE => FileEventKind::Create,
                            _CREATE_WRITE => FileEventKind::Create,
                            Op::REMOVE => FileEventKind::Delete,
                            Op::WRITE => FileEventKind::Update,
                            _x => {
                                continue;
                            }
                        };

                        let file_kind = match path.is_dir() {
                            true => FileSubKind::Dir,
                            false => FileSubKind::File,
                        };

                        let event = FileEvent {
                            path: path.to_str().unwrap().to_string(),
                            event_kind: event_kind,
                            file_kind: file_kind,
                        };

                        let event_tx = event_tx.clone();
                        tokio_runtime.block_on(async move {
                            event_tx.send(event).await;
                        })
                    }
                    Ok(event) => {
                        eprintln!("file_access broken event: {:?}", event);
                    }
                    Err(error) => {
                        eprintln!("WATCH ERROR: {}", error);
                        break;
                    }
                }
            }
        });
        Ok(event_rx)
    }
}
