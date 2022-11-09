use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter};
use std::io;
use std::num::ParseIntError;
use std::ops::Range;
use std::rc::Rc;
use std::string::FromUtf8Error;
use std::sync::{Arc, PoisonError};

use nom::error::VerboseError;
use nom::Err;
use nom_locate::LocatedSpan;
use nom_supreme::error::{ErrorTree, StackContext};
use serde::de::Error;
use tokio::sync::mpsc::error::{SendError, SendTimeoutError};
use tokio::sync::oneshot::error::RecvError;
use tokio::time::error::Elapsed;

use crate::err::report::{Label, Report, ReportKind};
use cosmic_nom::{Span, Trace};
use cosmic_nom::SpanExtra;

use crate::parse::error::find_parse_err;
use crate::substance::{FormErrs, Substance};
use crate::wave::core::http2::StatusCode;
use crate::wave::core::ReflectedCore;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum SpaceErr {
    Status { status: u16, message: String },
    ParseErrs(ParseErrs),
}

impl SpaceErr {
    pub fn print(&self) {
        match self {
            SpaceErr::Status { status, message } => {
                println!("{}: {}", status, message);
            }
            SpaceErr::ParseErrs(errs) => {
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

impl Into<ReflectedCore> for SpaceErr {
    fn into(self) -> ReflectedCore {
        println!("SpaceErr -> ReflectedCore");
        match self {
            SpaceErr::Status { status, .. } => ReflectedCore {
                headers: Default::default(),
                status: StatusCode::from_u16(status).unwrap_or(StatusCode::from_u16(500).unwrap()),
                body: Substance::Err(self),
            },
            SpaceErr::ParseErrs(_) => ReflectedCore {
                headers: Default::default(),
                status: StatusCode::from_u16(500u16).unwrap_or(StatusCode::from_u16(500).unwrap()),
                body: Substance::Err(self),
            },
        }
    }
}

pub trait CoreReflector {
    fn as_reflected_core(self) -> ReflectedCore;
}

impl CoreReflector for SpaceErr {
    fn as_reflected_core(self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(self.status()).unwrap(),
            body: Substance::Err(self),
        }
    }
}

impl SpaceErr {
    pub fn str<S: ToString>(s: S) -> SpaceErr {
        SpaceErr::new(500, s)
    }

    pub fn map<S>(s: S) -> Self
    where
        S: ToString,
    {
        SpaceErr::new(500, s)
    }

    pub fn from_status(status: u16) -> SpaceErr {
        let message = match status {
            400 => "Bad Request".to_string(),
            404 => "Not Found".to_string(),
            403 => "Forbidden".to_string(),
            408 => "Timeout".to_string(),
            500 => "Internal Server Error".to_string(),
            status => format!("{} Error", status),
        };
        SpaceErr::Status { status, message }
    }
}

/*
impl Into<ParseErrs> for UniErr {
    fn into(self) -> ParseErrs {
        match self {
            UniErr::Status { status, message } => {
                let mut builder = Report::build(ReportKind::Error, (), 0);
                let report = builder.with_message(message).finish();
                let errs = ParseErrs {
                    report: vec![report],
                    source: None,
                };
                errs
            }
            UniErr::ParseErrs(errs) => errs,
        }
    }
}

 */

impl SpaceErr {
    pub fn timeout<S: ToString>(s: S) -> Self {
        SpaceErr::new(408, format!("Timeout: {}", s.to_string()))
    }

    pub fn server_error<S: ToString>(s: S) -> Self {
        SpaceErr::new(500, format!("Server Side Error: {}", s.to_string()))
    }
    pub fn forbidden<S: ToString>(s: S) -> Self {
        SpaceErr::new(403, format!("Forbidden: {}", s.to_string()))
    }

    pub fn not_found<S: ToString>(s: S) -> Self {
        SpaceErr::new(404, format!("Not Found: {}", s.to_string()))
    }

    pub fn bad_request<S: ToString>(s: S) -> Self {
        SpaceErr::new(400, format!("Bad Request: {}", s.to_string()))
    }

    pub fn ctx<S: ToString>(mut self, ctx: S) -> Self {
        match self {
            Self::Status { status, message } => Self::Status {
                status,
                message: format!("{} | {}", message, ctx.to_string()),
            },
            Self::ParseErrs(mut errs) => Self::ParseErrs(errs.ctx(ctx)),
        }
    }
}

impl SpaceErr {
    pub fn new<S: ToString>(status: u16, message: S) -> Self {
        if message.to_string().as_str() == "500" {
            panic!("500 err message");
        }

        Self::Status {
            status,
            message: message.to_string(),
        }
    }
}

impl StatusErr for SpaceErr {
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

pub trait StatusErr {
    fn status(&self) -> u16;
    fn message(&self) -> String;
}

impl Display for SpaceErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SpaceErr::Status { status, message } => {
                f.write_str(format!("{}: {}", status, message).as_str())
            }
            SpaceErr::ParseErrs(errs) => {
                println!("PRINING!");
                self.print();
                f.write_str("Error Report...")
            }
        }
    }
}

impl std::error::Error for SpaceErr {}

impl<C> From<SendTimeoutError<C>> for SpaceErr {
    fn from(e: SendTimeoutError<C>) -> Self {
        SpaceErr::Status {
            status: 500,
            message: e.to_string(),
        }
    }
}

impl<C> From<tokio::sync::mpsc::error::SendError<C>> for SpaceErr {
    fn from(e: SendError<C>) -> Self {
        SpaceErr::server_error(e.to_string())
    }
}

impl<C> From<tokio::sync::broadcast::error::SendError<C>> for SpaceErr {
    fn from(e: tokio::sync::broadcast::error::SendError<C>) -> Self {
        SpaceErr::server_error(e.to_string())
    }
}

impl From<tokio::sync::watch::error::RecvError> for SpaceErr {
    fn from(e: tokio::sync::watch::error::RecvError) -> Self {
        SpaceErr::server_error(e.to_string())
    }
}

impl From<String> for SpaceErr {
    fn from(message: String) -> Self {
        Self::Status {
            status: 500,
            message,
        }
    }
}

impl From<Elapsed> for SpaceErr {
    fn from(e: Elapsed) -> Self {
        Self::Status {
            status: 408,
            message: e.to_string(),
        }
    }
}

impl<T> From<PoisonError<T>> for SpaceErr {
    fn from(e: PoisonError<T>) -> Self {
        SpaceErr::Status {
            status: 500,
            message: e.to_string(),
        }
    }
}

impl From<FromUtf8Error> for SpaceErr {
    fn from(message: FromUtf8Error) -> Self {
        Self::Status {
            status: 500,
            message: message.to_string(),
        }
    }
}

impl From<&str> for SpaceErr {
    fn from(message: &str) -> Self {
        Self::Status {
            status: 500,
            message: message.to_string(),
        }
    }
}

impl From<Box<bincode::ErrorKind>> for SpaceErr {
    fn from(message: Box<bincode::ErrorKind>) -> Self {
        Self::Status {
            status: 500,
            message: message.to_string(),
        }
    }
}

impl From<Infallible> for SpaceErr {
    fn from(i: Infallible) -> Self {
        Self::Status {
            status: 500,
            message: i.to_string(),
        }
    }
}

impl From<nom::Err<VerboseError<&str>>> for SpaceErr {
    fn from(error: nom::Err<VerboseError<&str>>) -> Self {
        Self::Status {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<semver::Error> for SpaceErr {
    fn from(error: semver::Error) -> Self {
        Self::Status {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<ErrorTree<&str>> for SpaceErr {
    fn from(error: ErrorTree<&str>) -> Self {
        Self::Status {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<strum::ParseError> for SpaceErr {
    fn from(error: strum::ParseError) -> Self {
        Self::Status {
            status: 500,
            message: error.to_string(),
        }
    }
}

impl From<()> for SpaceErr {
    fn from(err: ()) -> Self {
        Self::Status {
            status: 500,
            message: "Empty Error".to_string(),
        }
    }
}

impl From<tokio::sync::oneshot::error::RecvError> for SpaceErr {
    fn from(err: RecvError) -> Self {
        Self::Status {
            status: 500,
            message: err.to_string(),
        }
    }
}

impl From<ParseIntError> for SpaceErr {
    fn from(x: ParseIntError) -> Self {
        Self::Status {
            status: 500,
            message: x.to_string(),
        }
    }
}

impl From<regex::Error> for SpaceErr {
    fn from(x: regex::Error) -> Self {
        Self::Status {
            status: 500,
            message: x.to_string(),
        }
    }
}

/*
impl From<ToStrError> for UniErr {
    fn from(x: ToStrError) -> Self {
        Self::Status {
            status: 500,
            message: x.to_string(),
        }
    }
}

 */

impl<I: Span> From<nom::Err<ErrorTree<I>>> for SpaceErr {
    fn from(err: Err<ErrorTree<I>>) -> Self {
        fn handle<I: Span>(err: ErrorTree<I>) -> SpaceErr {
            match err {
                ErrorTree::Base {
                    location,
                    kind: _kind,
                } => SpaceErr::Status {
                    status: 500,
                    message: format!(
                        "parse error line: {} column: {}",
                        location.location_line(),
                        location.get_column()
                    ),
                },
                ErrorTree::Stack { base, contexts } => match contexts.first() {
                    None => SpaceErr::Status {
                        status: 500,
                        message: "error, cannot find location".to_string(),
                    },
                    Some((location, _)) => SpaceErr::Status {
                        status: 500,
                        message: format!(
                            "Stack parse error line: {} column: {}",
                            location.location_line(),
                            location.get_column()
                        ),
                    },
                },
                ErrorTree::Alt(what) => SpaceErr::Status {
                    status: 500,
                    message: "alt error".to_string(),
                },
            }
        }
        match err {
            Err::Incomplete(_) => SpaceErr::Status {
                status: 500,
                message: "unexpected incomplete parsing error".to_string(),
            },

            Err::Error(err) => handle(err),
            Err::Failure(err) => handle(err),
        }
    }
}

impl Into<String> for SpaceErr {
    fn into(self) -> String {
        self.to_string()
    }
}

impl From<io::Error> for SpaceErr {
    fn from(e: io::Error) -> Self {
        SpaceErr::new(500, e.to_string().as_str())
    }
}

impl From<ParseErrs> for SpaceErr {
    fn from(errs: ParseErrs) -> Self {
        SpaceErr::ParseErrs(errs)
    }
}
impl<I: Span> From<nom::Err<ErrorTree<I>>> for ParseErrs {
    fn from(err: Err<ErrorTree<I>>) -> Self {
        match find_parse_err(&err) {
            SpaceErr::Status { .. } => ParseErrs {
                report: vec![],
                source: None,
                ctx: "".to_string(),
            },
            SpaceErr::ParseErrs(parse_errs) => parse_errs,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ParseErrs {
    pub report: Vec<Report>,
    pub source: Option<Arc<String>>,
    pub ctx: String,
}



impl ParseErrs {
    pub fn ctx<S: ToString>(mut self, ctx: S) -> Self {
        Self {
            report: self.report,
            source: self.source,
            ctx: ctx.to_string(),
        }
    }

    pub fn from_report(report: Report, source: Arc<String>) -> Self {
        Self {
            report: vec![report],
            source: Some(source),
            ctx: "".to_string(),
        }
    }

    pub fn from_loc_span<I: Span>(message: &str, label: &str, span: I) -> SpaceErr {
        let mut builder = Report::build(ReportKind::Error, (), 23);
        let report = builder
            .with_message(message)
            .with_label(
                Label::new(span.location_offset()..(span.location_offset() + span.len()))
                    .with_message(label),
            )
            .finish();
        return ParseErrs::from_report(report, span.extra()).into();
    }

    pub fn from_range(
        message: &str,
        label: &str,
        range: Range<usize>,
        extra: SpanExtra,
    ) -> SpaceErr {
        let mut builder = Report::build(ReportKind::Error, (), 23);
        let report = builder
            .with_message(message)
            .with_label(Label::new(range).with_message(label))
            .finish();
        return ParseErrs::from_report(report, extra).into();
    }

    pub fn from_trace(
        message: &str,
        label: &str,
        trace: &Trace
    ) -> SpaceErr {
        Self::from_range(message,label,trace.range.clone(), trace.extra.clone() )
    }

    pub fn from_owned_span<I: Span>(message: &str, label: &str, span: I) -> SpaceErr {
        let mut builder = Report::build(ReportKind::Error, (), 23);
        let report = builder
            .with_message(message)
            .with_label(
                Label::new(span.location_offset()..(span.location_offset() + span.len()))
                    .with_message(label),
            )
            .finish();
        return ParseErrs::from_report(report, span.extra()).into();
    }

    pub fn fold<E: Into<ParseErrs>>(errs: Vec<E>) -> ParseErrs {
        let errs: Vec<ParseErrs> = errs.into_iter().map(|e| e.into()).collect();

        let source = if let Some(first) = errs.first() {
            if let Some(source) = first.source.as_ref().cloned() {
                Some(source)
            } else {
                None
            }
        } else {
            None
        };

        let mut rtn = ParseErrs {
            report: vec![],
            source,
            ctx: "".to_string(),
        };

        for err in errs {
            for report in err.report {
                rtn.report.push(report)
            }
        }
        rtn
    }
}

impl From<SpaceErr> for ParseErrs {
    fn from(u: SpaceErr) -> Self {
        ParseErrs {
            report: vec![],
            source: None,
            ctx: "".to_string(),
        }
    }
}

impl From<serde_urlencoded::de::Error> for SpaceErr {
    fn from(err: serde_urlencoded::de::Error) -> Self {
        SpaceErr::Status {
            status: 500u16,
            message: err.to_string(),
        }
    }
}

impl From<serde_urlencoded::ser::Error> for SpaceErr {
    fn from(err: serde_urlencoded::ser::Error) -> Self {
        SpaceErr::Status {
            status: 500u16,
            message: err.to_string(),
        }
    }
}

pub mod report {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Report {
        kind: ReportKind,
        code: Option<String>,
        msg: Option<String>,
        note: Option<String>,
        help: Option<String>,
        location: Range,
        labels: Vec<Label>,
    }

    impl Into<ariadne::Report> for Report {
        fn into(self) -> ariadne::Report {
            let mut builder = ariadne::Report::build(self.kind.into(), (), 0);
            if let Some(msg) = self.msg {
                builder.set_message(msg);
            }
            for label in self.labels {
                builder.add_label(label.into());
            }
            builder.finish()
        }
    }

    impl Default for Report {
        fn default() -> Self {
            Self {
                kind: ReportKind::Error,
                code: None,
                msg: None,
                note: None,
                help: None,
                location: Range { start: 0, end: 0 },
                labels: vec![],
            }
        }
    }

    pub struct ReportBuilder {
        kind: ReportKind,
        code: Option<String>,
        msg: Option<String>,
        note: Option<String>,
        help: Option<String>,
        location: Range,
        labels: Vec<Label>,
    }

    impl ReportBuilder {
        pub fn with_message<S: ToString>(mut self, message: S) -> Self {
            self.msg.replace(message.to_string());
            self
        }

        pub fn with_label(mut self, label: Label) -> Self {
            self.labels.push(label);
            self
        }

        pub fn finish(self) -> Report {
            Report {
                kind: self.kind,
                code: None,
                msg: self.msg,
                note: None,
                help: None,
                location: self.location,
                labels: self.labels,
            }
        }
    }

    impl Report {
        pub(crate) fn build(kind: ReportKind, p1: (), p2: i32) -> ReportBuilder {
            ReportBuilder {
                kind,
                code: None,
                msg: None,
                note: None,
                help: None,
                location: Default::default(),
                labels: vec![],
            }
        }
    }

    #[derive(Serialize, Deserialize, Copy, Clone, Debug, PartialEq, Eq)]
    pub enum ReportKind {
        Error,
        Warning,
        Advice,
    }

    impl Into<ariadne::ReportKind> for ReportKind {
        fn into(self) -> ariadne::ReportKind {
            match self {
                ReportKind::Error => ariadne::ReportKind::Error,
                ReportKind::Warning => ariadne::ReportKind::Warning,
                ReportKind::Advice => ariadne::ReportKind::Advice,
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Range {
        pub start: u32,
        pub end: u32,
    }

    impl Into<std::ops::Range<usize>> for Range {
        fn into(self) -> std::ops::Range<usize> {
            std::ops::Range {
                start: self.start as usize,
                end: self.end as usize,
            }
        }
    }

    impl Default for Range {
        fn default() -> Self {
            Self { start: 0, end: 0 }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Label {
        span: Range,
        msg: Option<String>,
        color: Option<Color>,
        order: i32,
        priority: i32,
    }

    impl Into<ariadne::Label> for Label {
        fn into(self) -> ariadne::Label {
            let mut rtn = ariadne::Label::new(self.span.into());
            if let Some(msg) = self.msg {
                rtn = rtn.with_message(msg);
            }
            rtn
        }
    }

    impl Label {
        pub fn new(range: std::ops::Range<usize>) -> Self {
            Self {
                span: Range {
                    start: range.start as u32,
                    end: range.end as u32,
                },
                msg: None,
                color: None,
                order: 0,
                priority: 0,
            }
        }

        pub fn with_message(mut self, msg: &str) -> Label {
            self.msg.replace(msg.to_string());
            self
        }
    }

    #[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
    pub enum Color {
        /// No color has been set. Nothing is changed when applied.
        Unset,

        /// Terminal default #9. (foreground code `39`, background code `49`).
        Default,

        /// Black #0 (foreground code `30`, background code `40`).
        Black,

        /// Red: #1 (foreground code `31`, background code `41`).
        Red,

        /// Green: #2 (foreground code `32`, background code `42`).
        Green,

        /// Yellow: #3 (foreground code `33`, background code `43`).
        Yellow,

        /// Blue: #4 (foreground code `34`, background code `44`).
        Blue,

        /// Magenta: #5 (foreground code `35`, background code `45`).
        Magenta,

        /// Cyan: #6 (foreground code `36`, background code `46`).
        Cyan,

        /// White: #7 (foreground code `37`, background code `47`).
        White,

        /// A color number from 0 to 255, for use in 256-color terminals.
        Fixed(u8),

        /// A 24-bit RGB color, as specified by ISO-8613-3.
        RGB(u8, u8, u8),
    }
}

#[cfg(test)]
pub mod test {

    #[test]
    pub fn compile() {}
}
