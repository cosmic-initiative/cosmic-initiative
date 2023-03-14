pub mod bind;

use std::ops::{Index, Range};
use std::str::FromStr;
use ariadne::{Label, Report, ReportKind};
use chumsky::{Error, text};
use chumsky::combinator::Map;
use chumsky::error::{Simple, SimpleReason};
use chumsky::prelude::{just, recursive, Parser, take_until, end};
use chumsky::primitive::filter;
use chumsky::text::{Character, TextParser};
use regex::Regex;
use semver::Version;
use crate::bind::Bind;
use crate::Stmt::Expr;

struct Doc {
    source: String,
    selectors: Vec<Selector>
}



#[derive(Clone, Debug, PartialEq)]
enum Selector {
    Routes(Vec<Route>),
    Invalid{
        name: CamelCase,
        span: Range<usize>
    }
}



#[derive(Clone, Debug, PartialEq)]
enum Route {
    PathToPath{ from: Path, to: Path, arrow: Arrow }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    pub path: String,
    pub absolute: bool
}




#[derive(Clone, Debug, PartialEq)]
enum Stmt {
    Expr,
    Loop(Vec<Stmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arrow {

}

#[derive(Clone, Debug, PartialEq)]
pub struct PathMapping {
    pub from: Path,
    pub to: Path,
    pub arrow: Arrow
}

#[derive(Clone, Debug, PartialEq)]
pub struct CamelCase{
    value: String
}

impl CamelCase {
   pub fn new(string: String) -> Result<Self,()> {
       let first = string.chars().nth(0).ok_or_else(||())?;
       if !(first.is_alphabetic() &&  first.is_uppercase() ) {
           return Err(());
       }
       for index in 1..string.len() {
           let c= string.chars().nth(index).ok_or_else(||())?;
           if !c.is_alphanumeric() {
               return Err(());
           }
       }
       Ok(CamelCase {
           value: string
       })
   }
}

impl ToString for CamelCase {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl FromStr for CamelCase {
    type Err = Vec<Simple<char>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(camel_case().then_ignore(end()).parse(s)?)
    }
}

pub struct EvalErr {
    pub span: Range<usize>,
    pub msg: String
}

impl EvalErr {
    pub fn new<S: ToString>( span: Range<usize>, msg: S) -> Self {
        Self {
            span,
            msg: msg.to_string()
        }
    }
}

fn selector() -> impl Parser<char, Selector, Error=Simple<char>> {
    camel_case().padded().then(route_lines().delimited_by(just('{'),just('}'))).map_with_span( |(name,routes), span| {
        match name.value.as_str() {
            "Routes" => Selector::Routes(routes),
            _ => Selector::Invalid {
                name,
                span
            }
        }
    })
}

fn selectors() -> impl Parser<char, Vec<Selector>, Error=Simple<char>> {
    selector().padded().repeated()
}






fn cap() -> impl Parser<char, char, Error=Simple<char>> {
   filter( |c:&char| c.is_alphabetic() && c.is_uppercase())
}

fn alphanumeric() -> impl Parser<char, char, Error=Simple<char>> {
    filter( |c:&char| c.is_alphanumeric() )
}

fn camel_case() -> impl Parser<char, CamelCase, Error=Simple<char>> {
    cap().then(alphanumeric().repeated()).map( |(first, chars)| {
        let mut string = String::from_iter(chars.iter());
        string.insert(0,first);
        CamelCase::new(string).unwrap()
    })
}

fn path() -> impl Parser<char, Path, Error=Simple<char>> {
    filter( |c:&char| c.is_alphanumeric()
                      || *c == '/'
                      || *c == '_'
                      || *c == '-'
                      || *c == '*'
    ).repeated().map( |path| {
        let mut string = String::new();

        for c in path{
            string.push(c);
        }

        Path{absolute: string.starts_with('/'),path:string}
    } )
}

fn arrow<'a>() -> impl Parser<char,Arrow,Error=Simple<char>> {
   just("->").map( |c| Arrow {} )
}

fn route<'a>() -> impl Parser<char, Route, Error=Simple<char>> {
    path().then(arrow().padded()).then(path()).map(|((from,arrow),to)| {
        Route::PathToPath { from, arrow, to }
    })
}

fn route_line<'a>() -> impl Parser<char, Route, Error=Simple<char>> {
   route().then_ignore(just(";").padded())
}

fn route_lines() -> impl Parser<char, Vec<Route>, Error=Simple<char>> {
    route_line().repeated().padded()
}


fn parser<'a>() -> impl Parser<char, Stmt, Error=Simple<char>> {
    route().map( |_| Stmt::Expr)
}

fn report_parse<'a>(errs: Vec<Simple<char>>) -> Report<'a> {
    let mut builder = Report::build(ReportKind::Error, (), 0);
    for err in errs {
        builder = builder.with_label(Label::new( err.span()).with_message(err.to_string()) );
    }
    builder.finish()
}

fn report_eval<'a>(errs: Vec<EvalErr> )-> Report<'a> {
    let mut builder = Report::build(ReportKind::Error, (), 0);
    for err in errs {
        builder = builder.with_label(Label::new( err.span.clone()).with_message(err.msg.clone()) );
    }
    builder.finish()
}


fn eval( selectors: Vec<Selector>) -> Result<Bind,EvalErr> {
    for s in selectors {
        if let Selector::Invalid {name,span} = s {
            let err = EvalErr::new(span, format!("invalid selector name \"{}\"", name.value));
            return Err(err);
        }
    }
    Ok(Bind{})
}

#[cfg(test)]
pub mod test {
    use std::path::Path;
    use std::str::FromStr;
    use ariadne::{Label, Report, ReportKind, Source};
    use crate::{camel_case, CamelCase, eval, parser, path, report_eval, report_parse, route, Route, route_line, route_lines, Selector};
    use chumsky::{Parser, Stream};
    use chumsky::text::TextParser;

    #[test]
    pub fn selector() {
        let routes = r#"Routes {
            /hello -> /kitty;
            /goodnight -> /moon;
        }"#;
        let routes = crate::selector().parse(routes).unwrap();
        if let Selector::Routes(routes) = routes {
            assert_eq!(routes.len(),2);
        } else {
            assert!(false);
        }
    }

    #[test]
    pub fn selector_fail() {
        let routes = r#"Zomigs {
            /hello -> /kitty;
            /goodnight -> /moon;
        }"#;
        let selectors= crate::selectors().parse(routes).unwrap();
        let err = eval(selectors).unwrap_err();;
        let errs = vec![err];
        report_eval(errs).print(ariadne::Source::from(routes));
    }



    #[test]
    pub fn came_case() {
        assert!(CamelCase::from_str("CamelCase123").is_ok());
        assert!(CamelCase::from_str("CamelCase123 ").is_err());
        assert!(CamelCase::from_str("camelCase123").is_err());
        assert!(CamelCase::from_str("Camel Case123").is_err());
        let camel = CamelCase::from_str("CamelCase123").unwrap();
        assert_eq!(camel.value.as_str(),"CamelCase123")
    }

    #[test]
    pub fn route_pass<'a>() {
        let mapping = route().parse( "hello -> /kitty").unwrap();
        if let Route::PathToPath {from,to,arrow}= mapping {
            assert_eq!(from.path, "hello".to_string());
            assert!(!from.absolute);
            assert_eq!(to.path, "/kitty".to_string());
            assert!(to.absolute);
        }
    }


    #[test]
    pub fn routes_lines_pass<'a>() {
        let lines = r#"

        hello -> /kitty;

        /going-for-broke -> /my/going-for-broke;

        "#;
        let mappings = route_lines().parse( lines).unwrap();
        assert_eq!( mappings.len(), 2 );
    }



    #[test]
    pub fn path_pass<'a>()  {
        let stmts = path().parse(
            r#"/root/filesys"#,
        ).unwrap();
    }


}