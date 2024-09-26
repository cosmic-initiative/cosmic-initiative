use serde::{Deserialize, Serialize};
use thiserror::Error;
use crate::space::err::{ParseErrs, SpaceErr};
use crate::space::substance::Substance;
use crate::space::thiserr;
use crate::space::wave::core::http2::StatusCode;
use crate::space::wave::core::{CoreReflector, ReflectedCore};
#[derive(Debug, Clone, Serialize, Deserialize,Error,Display)]
pub enum ThisErr {
    String(#[from] String),
    Status { status: u16, message: String },
    ParseErrs(#[from] ParseErrs),
    Error( #[from] Box<dyn std::error::Error+Send+Sync>),
    TokioRecvErr( #[from] tokio::sync::oneshot::error::RecvError)
}

pub fn err<E>( e: E ) -> ThisErr where E:ToString{
    ThisErr::String(e.to_string())
}

impl ThisErr {
    pub fn new( status: u16, message: String ) -> Self {
        Self::Status {
            status,
            message
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

impl ThisErr{

    pub fn str<S: ToString>(s: S) -> ThisErr {
        ThisErr::new(500, s)
    }

    pub fn map<S>(s: S) -> Self
    where
        S: ToString,
    {
        ThisErr::new(500u16, s)
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
    fn status(&self) -> u16 {
        match self {
            SpaceErr::Status { status, .. } => status.clone(),
            SpaceErr::ParseErrs(_) => 500u16,
        }
    }

    fn message(&self) -> String {
        match self {
            SpaceErr::Status { status, message } => message.clone(),
            SpaceErr::ParseErrs(_) => "Error report".to_string(),
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
