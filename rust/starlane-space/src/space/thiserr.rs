use crate::space::err::{ParseErrs};
use crate::space::substance::Substance;
use crate::space::wave::core::http2::StatusCode;
use crate::space::wave::core::{CoreReflector, ReflectedCore};
use serde::{Deserialize, Serialize};
use thiserror::Error;
#[derive(Debug, Clone, Error,Display)]
pub enum ThisErr {
    String(#[from] String),
    Status { status: u16, message: String },
    ParseErrs(#[from] ParseErrs),
    TokioRecvErr( #[from] tokio::sync::oneshot::error::RecvError)
}

pub fn err<I,O>( e: I ) -> ThisErr where I:ToString, O: std::error::Error {
    ThisErr::String(e.to_string())
}

impl ThisErr {
    pub fn new<T: ToString>( status: u16, message: T) -> Self {
        Self::Status {
            status,
            message: message.to_string()
        }
    }
}


impl Into<ReflectedCore> for ThisErr {
    fn into(self) -> ReflectedCore {
        println!("ThisErr -> ReflectedCore({})",self.to_string());
        match self {
            ThisErr::Status { status, .. } => ReflectedCore {
                headers: Default::default(),
                status: StatusCode::from_u16(status).unwrap_or(StatusCode::from_u16(500).unwrap()),
                body: Substance::Err(self),
            },
            ThisErr::ParseErrs(_) => ReflectedCore {
                headers: Default::default(),
                status: StatusCode::from_u16(500u16).unwrap_or(StatusCode::from_u16(500).unwrap()),
                body: Substance::Err(self),
            },
        }
    }
}



impl ThisErr{
    pub fn print(&self) {
        match self {
            ThisErr::Status { status, message } => {
                println!("{}: {}", status, message);
            }
            ThisErr::ParseErrs(errs) => {
                println!("REport len: {}", errs.report.len());
                for report in &errs.report {
                    let report: ariadne::Report = report.clone().into();
                    if let Some(source) = &errs.source {
                        let source = source.to_string();
                        report.print(ariadne::Source::from(source));
                    } else {
                        println!("No source..");
                    }
                }
            }
        }
    }
}

impl CoreReflector for ThisErr {
    fn as_reflected_core(self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(self.status()).unwrap(),
            body: Substance::Err(self),
        }
    }
}






impl ThisErr {
    pub fn str<S: ToString>(s: S) -> ThisErr {
        ThisErr::new(500, s)
    }

    pub fn map<S>(s: S) -> Self
    where
        S: ToString,
    {
        ThisErr::new(500, s)
    }

    pub fn from_status(status: u16) -> ThisErr {
        let message = match status {
            400 => "Bad Request".to_string(),
            404 => "Not Found".to_string(),
            403 => "Forbidden".to_string(),
            408 => "Timeout".to_string(),
            500 => "Internal Server Error".to_string(),
            status => format!("{} Error", status),
        };
        ThisErr::Status { status, message }
    }
}
