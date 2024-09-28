use core::fmt::Pointer;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;
use std::string::FromUtf8Error;
use std::sync::Arc;

use crate::space::err::report::{Label, Report, ReportKind};
use starlane_parse::Span;
use starlane_parse::SpanExtra;

use serde::{Deserialize, Serialize};


pub type Result<T> = anyhow::Result<T>;
pub use anyhow::anyhow as err;
pub use anyhow::Error as Error;

#[derive(thiserror::Error, Debug,strum_macros::Display)]
pub enum SpaceErr {
    Status { status: u16, message: String },
    ParseErrs(#[from]  ParseErrs),
    FromUtf8Error(#[from] FromUtf8Error),
    String(String)
}

impl SpaceErr {

    pub fn status(&self) -> u16 {
        match self {
            Self::Status { status, .. } => status.clone(),
            _ => 500u16,
        }
    }

    pub fn from_str<S: ToString>( s: S) -> Self {
        Self::String(s.to_string())
    }
    pub fn not_found<S: ToString>(message: S) -> Self {
        Self::new(404u16,message)
    }
    pub fn new<S: ToString>( status: u16, message: S) -> err::Error {
       Self::Status { status: status, message: message.to_string() }
    }
}

/*
impl<I: Span> From<nom::Err<ErrorTree<I>>> for ParseErrs {
    fn from(err: Err<ErrorTree<I>>) -> Self {
        match find_parse_err(&err) {
            err::Error::Status { .. } => ParseErrs {
                report: vec![],
                source: None,
                ctx: "".to_string(),
            },
            err::Error::ParseErrs(parse_errs) => parse_errs,
        }
    }
}
 */

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, thiserror::Error)]
pub struct ParseErrs {
    pub report: Vec<Report>,
    pub source: Option<Arc<String>>,
    pub ctx: String,
}

impl Display for ParseErrs {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "parse errors")
    }
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

    pub fn from_loc_span<I: Span>(message: &str, label: &str, span: I) -> anyhow::Error{
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
    ) -> err::Error {
        let mut builder = Report::build(ReportKind::Error, (), 23);
        let report = builder
            .with_message(message)
            .with_label(Label::new(range).with_message(label))
            .finish();
        return ParseErrs::from_report(report, extra).into();
    }

    pub fn from_owned_span<I: Span>(message: &str, label: &str, span: I) -> err::Error {
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

impl From<err::Error> for ParseErrs {
    fn from(u: err::Error) -> Self {
        ParseErrs {
            report: vec![],
            source: None,
            ctx: "".to_string(),
        }
    }
}

impl From<serde_urlencoded::de::Error> for err::Error {
    fn from(err: serde_urlencoded::de::Error) -> Self {
        err::Error::Status {
            status: 500u16,
            message: err.to_string(),
        }
    }
}

impl From<serde_urlencoded::ser::Error> for err::Error {
    fn from(err: serde_urlencoded::ser::Error) -> Self {
        err::Error::Status {
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
