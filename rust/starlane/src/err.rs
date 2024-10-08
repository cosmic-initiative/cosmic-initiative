use std::convert::Infallible;
use thiserror::Error;
use tokio::io;
use tokio::sync::oneshot::error::RecvError;
use crate::hyperspace::err::{ErrKind, HyperErr};

#[cfg(feature = "postgres")]
use crate::registry::postgres::err::PostErr;

#[derive(Error,Debug)]
pub enum ThisErr {
    #[error("star error {0}")]
    StarErr(StarErr),
     #[error("{0}")]
    String(String),
    #[error("{0}")]
    TokioIo(#[from] io::Error),
    #[error("{0}")]
    Iniff(#[from] Infallible),
     #[error("{0}")]
     RecvErr(#[from] RecvError),
    #[error("{0}")]
    StripPrefix(#[from] std::path::StripPrefixError),
}

impl StarErr {
 pub fn as_display(&self) -> String {
     self.to_string()
 }
}

impl From<&str> for ThisErr {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}


#[derive(Debug, Clone)]
pub struct StarErr {
    pub kind: ErrKind,
    pub message: String,
}

impl From<ThisErr> for StarErr {
    fn from(value: ThisErr) -> Self {
        StarErr::new( value.to_string())
    }
}



pub mod convert {
    use starlane_space as starlane;
    use crate::err::StarErr;
    use crate::hyperspace::err::{ErrKind, HyperErr};
    use ascii::FromAsciiError;
    use std::io;
    use std::str::Utf8Error;
    use std::string::FromUtf8Error;
    use tokio::sync::oneshot;
    use tokio::time::error::Elapsed;
    use wasmer::{CompileError, ExportError, InstantiationError, RuntimeError};
    use starlane::space::err::SpaceErr;

    impl From<strum::ParseError> for StarErr {
        fn from(e: strum::ParseError) -> Self {
            Self {
                kind: ErrKind::Default,
                message: e.to_string(),
            }
        }
    }

    impl From<url::ParseError> for StarErr {
        fn from(e: url::ParseError) -> Self {
            Self {
                kind: ErrKind::Default,
                message: e.to_string(),
            }
        }
    }
    impl From<FromAsciiError<std::string::String>> for StarErr {
        fn from(e: FromAsciiError<String>) -> Self {
            Self {
                kind: ErrKind::Default,
                message: e.to_string(),
            }
        }
    }

    impl ToString for StarErr {
        fn to_string(&self) -> String {
            self.message.clone()
        }
    }

    impl From<()> for StarErr {
        fn from(_: ()) -> Self {
            StarErr::new("empty")
        }
    }

    impl From<sqlx::Error> for StarErr {
        fn from(e: sqlx::Error) -> Self {
            StarErr::new(e)
        }
    }

    impl Into<SpaceErr> for StarErr {
        fn into(self) -> SpaceErr {
            SpaceErr::server_error(self.to_string())
        }
    }

    impl From<oneshot::error::RecvError> for StarErr {
        fn from(err: oneshot::error::RecvError) -> Self {
            StarErr::new(err)
        }
    }

    impl From<Elapsed> for StarErr {
        fn from(err: Elapsed) -> Self {
            StarErr::new(err)
        }
    }

    impl From<String> for StarErr {
        fn from(err: String) -> Self {
            StarErr::new(err)
        }
    }

    impl From<&'static str> for StarErr {
        fn from(err: &'static str) -> Self {
            StarErr::new(err)
        }
    }

    impl From<SpaceErr> for StarErr {
        fn from(err: SpaceErr) -> Self {
            StarErr::new(err)
        }
    }

    impl From<io::Error> for StarErr {
        fn from(err: io::Error) -> Self {
            StarErr::new(err)
        }
    }

    impl From<zip::result::ZipError> for StarErr {
        fn from(a: zip::result::ZipError) -> Self {
            StarErr::new(a)
        }
    }

    impl From<Box<bincode::ErrorKind>> for StarErr {
        fn from(e: Box<bincode::ErrorKind>) -> Self {
            StarErr::new(e)
        }
    }

    impl From<ExportError> for StarErr {
        fn from(e: ExportError) -> Self {
            StarErr::new(e)
        }
    }

    impl From<Utf8Error> for StarErr {
        fn from(e: Utf8Error) -> Self {
            StarErr::new(e)
        }
    }

    impl From<FromUtf8Error> for StarErr {
        fn from(e: FromUtf8Error) -> Self {
            StarErr::new(e)
        }
    }

    impl From<InstantiationError> for StarErr {
        fn from(_: InstantiationError) -> Self {
            todo!()
        }
    }

    impl From<CompileError> for StarErr {
        fn from(e: CompileError) -> Self {
            StarErr::new(e)
        }
    }

    impl From<RuntimeError> for StarErr {
        fn from(e: RuntimeError) -> Self {
            StarErr::new(e)
        }
    }
}
