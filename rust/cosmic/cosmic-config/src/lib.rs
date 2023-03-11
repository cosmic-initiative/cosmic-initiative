use std::ops::Index;
use chumsky::{Error, text};
use chumsky::combinator::Map;
use chumsky::error::{Simple, SimpleReason};
use chumsky::prelude::{just, recursive, Parser, take_until, end};
use chumsky::primitive::filter;
use chumsky::text::{Character, TextParser};
use regex::Regex;
use semver::Version;
use crate::Stmt::Expr;

struct Doc<'src> {
    t: &'src str,
    selectors: Vec<Selector<'src>>
}



#[derive(Clone, Debug, PartialEq)]
enum Selector<'src> {
    Http(Vec<Route<'src>>)
}

#[derive(Clone, Debug, PartialEq)]
enum Route<'src> {
    PathToPath{ from: &'src str, to: &'src str }
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

fn path_mapping<'a>() -> impl Parser<char, PathMapping, Error=Simple<char>> {
    path().then(arrow().padded()).then(path()).map(|((from,arrow),to)| {
        PathMapping {
            from,
            to,
            arrow
        }
    })
}


fn parser<'a>() -> impl Parser<char, Stmt, Error=Simple<char>> {
    path_mapping().map( |_| Stmt::Expr)
}

#[cfg(test)]
pub mod test {
    #![feature(unwrap_infallible)]
    use crate::{parser, path, path_mapping};
    use chumsky::{Parser, Stream};
    use chumsky::text::TextParser;

    #[test]
    pub fn path_mapping_pass<'a>() {
        let mapping = path_mapping().parse( "hello -> /kitty").unwrap();
        assert_eq!(mapping.from.path, "hello".to_string());
        assert!(!mapping.from.absolute);
        assert_eq!(mapping.to.path, "/kitty".to_string());
        assert!(mapping.to.absolute);
    }

    #[test]
    pub fn path_pass<'a>()  {
        let stmts = path().parse(
            r#"/root/filesys"#,
        ).unwrap();
    }

    #[test]
    pub fn path_fail<'a>()  {
        let stmts = path().parse(
            r#"/root/more "#,
        );
        assert!(stmts.is_err());
        for e in stmts.unwrap_err() {
            println!("{}", e );
        }
    }
}