pub mod point;
pub mod kind;
pub mod util;
pub mod command;

#[cfg(test)]
pub mod test2;

use core::fmt;
use core::fmt::Display;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::ops;
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

//use ariadne::{Label, Report, ReportKind};
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::bytes::complete::{escaped, is_a, is_not};
use nom::bytes::complete::{tag, take_till, take_until, take_until1, take_while};
use nom::character::complete::{
    alpha0, alphanumeric0, alphanumeric1, anychar, char, digit0, line_ending, multispace0,
    multispace1, newline, one_of, satisfy, space0, space1,
};
use nom::character::complete::{alpha1, digit1};
use nom::character::is_space;
use nom::combinator::{all_consuming, opt};
use nom::combinator::{cut, eof, fail, not, peek, recognize, success, value, verify};
use nom::error::{context, ContextError, ErrorKind, ParseError, VerboseError};
use nom::multi::{many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{
    AsChar, Compare, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset,
    Parser, Slice,
};
use nom::{Err, IResult};
use nom_locate::LocatedSpan;
use nom_supreme::error::ErrorTree;
use nom_supreme::final_parser::ExtractContext;
use nom_supreme::parser_ext::MapRes;
use nom_supreme::{parse_from_str, ParserExt};
use regex::internal::Input;
use regex::{Captures, Error, Match, Regex};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use cosmic_nom::{new_span, span_with_extra, Trace};
use cosmic_nom::{Res, Span, trim, tw, Wrap};

use crate::command::common::{PropertyMod, SetProperties, StateSrc, StateSrcVar};
use crate::command::direct::create::{
    Create, CreateVar, PointSegTemplate, PointTemplate, PointTemplateSeg,
    PointTemplateVar, Require, Strategy, Template, TemplateVar,
};
use crate::command::direct::get::{Get, GetOp, GetVar};
use crate::command::direct::select::{Select, SelectIntoSubstance, SelectVar};
use crate::command::direct::set::{Set, SetVar};
use crate::command::direct::CmdKind;
use crate::command::CommandVar;
use crate::config::bind::{
    BindConfig, Pipeline, PipelineStep, PipelineStepCtx, PipelineStepVar, PipelineStop,
    PipelineStopCtx, PipelineStopVar, RouteSelector, WaveDirection,
};
use crate::config::mechtron::MechtronConfig;
use crate::config::Document;
use crate::err::report::{Label, Report, ReportKind};
use crate::err::{ParseErrs, SpaceErr};
use crate::kind2::{Kind, KindCat, KindSelector, Pattern, Specific};
use crate::kind2::parse::{kind_selector, proto_kind};
use crate::loc::StarKey;
use crate::loc::{
    Layer, Point, PointCtx, PointSeg, PointSegCtx, PointSegDelim, PointSegment, PointSegVar,
    PointVar, RouteSeg, RouteSegVar, Surface, Topic, Uuid, Variable, VarVal, Version,
};
use crate::loc::parse::point_selector_var;
use crate::model::{CamelCase, Domain, SkewerCase};
use crate::parse::error::{find_parse_err, result};
use crate::parse::model::{
    BindScope, BindScopeKind, Block, BlockKind, Chunk, DelimitedBlockKind, LexBlock,
    LexParentScope, LexRootScope, LexScope, LexScopeSelector, LexScopeSelectorAndFilters,
    MechtronScope, MessageScopeSelectorAndFilters, NestedBlockKind, PipelineCtx, PipelineSegment,
    PipelineSegmentCtx, PipelineSegmentVar, PipelineVar, RootScopeSelector, RouteScope,
    ScopeFilterDef, ScopeFilters, ScopeFiltersDef, ScopeSelectorAndFiltersDef, Spanned, Subst,
    TerminatedBlockKind, TextType, Var, VarParser,
};
use crate::particle::{PointKind, PointKindVar};
use crate::security::{
    AccessGrantKind, AccessGrantKindDef, ChildPerms, ParticlePerms, Permissions, PermissionsMask,
    PermissionsMaskKind, Privilege,
};
use crate::selector::{ExactPointSeg, PatternBlockVar, PointHierarchy, PointKindSeg, PointSegSelector, SelectorDef, UploadBlock, VersionReq};
use crate::selector::specific::{ProductSelector, ProductVariantSelector, VendorSelector};

use crate::substance::Bin;
use crate::substance::{
    Call, CallCtx, CallKind, CallVar, CallWithConfig, CallWithConfigCtx, CallWithConfigVar,
    ExtCall, HttpCall, ListPattern, MapPattern, MapPatternCtx, MapPatternVar, NumRange, Substance,
    SubstanceFormat, SubstanceKind, SubstancePattern, SubstancePatternCtx, SubstancePatternVar,
    SubstanceTypePatternCtx, SubstanceTypePatternDef, SubstanceTypePatternVar,
};
use crate::util::{HttpMethodPattern, StringMatcher, ToResolved, ValuePattern};
use crate::wave::core::cmd::CmdMethod;
use crate::wave::core::ext::ExtMethod;
use crate::wave::core::http2::HttpMethod;
use crate::wave::core::hyp::HypMethod;
use crate::wave::core::MethodKind;
use crate::wave::core::{Method, MethodPattern};

/*
pub struct Parser {}

impl Parser {
    pub fn point(input: Span) -> Res<Span, Point> {
        point_subst(input)
    }

    pub fn consume_point(input: Span) -> Result<Point, ExtErr> {
        let (_, point) = all_consuming(point_subst)(input)?;
        Ok(point)
    }
}
 */

fn any_resource_path_segment<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '/')
                && !(char_item == '_')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

fn sys_route_chars<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '/')
                && !(char_item == '_')
                && !(char_item == ':')
                && !(char_item == '(')
                && !(char_item == ')')
                && !(char_item == '[')
                && !(char_item == ']')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn this_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    alt((recognize(tag(".")), recognize(not(other_route_segment))))(input)
        .map(|(next, _)| (next, RouteSeg::This))
}

pub fn local_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    tag("LOCAL")(input).map(|(next, _)| (next, RouteSeg::Local))
}

pub fn remote_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    tag("REMOTE")(input).map(|(next, _)| (next, RouteSeg::Remote))
}

pub fn global_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    tag("GLOBAL")(input).map(|(next, _)| (next, RouteSeg::Global))
}

pub fn domain_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    domain_chars(input).map(|(next, domain)| (next, RouteSeg::Domain(domain.to_string())))
}

pub fn tag_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    delimited(tag("["), skewer_chars, tag("]"))(input)
        .map(|(next, tag)| (next, RouteSeg::Tag(tag.to_string())))
}

pub fn hyper_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    delimited(tag("<<"), sys_route_chars, tag(">>"))(input)
        .map(|(next, tag)| (next, RouteSeg::Star(tag.to_string())))
}

pub fn other_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    alt((
        hyper_route_segment,
        tag_route_segment,
        domain_route_segment,
        global_route_segment,
        local_route_segment,
        remote_route_segment,
    ))(input)
}

pub fn point_route_segment<I: Span>(input: I) -> Res<I, RouteSeg> {
    alt((this_route_segment, other_route_segment))(input)
}

/*
pub fn point_segment(input: Span) -> Res<Span, PointSegCtx> {
    alt((
        base_point_segment,
        space_point_segment,
        version_point_segment,
        filesystem_point_segment,
        file_point_segment,
    ))(input)
}

 */

pub fn mesh_eos<I: Span>(input: I) -> Res<I, I> {
    peek(alt((tag(":"), eop)))(input)
}

pub fn fs_trailing<I: Span>(input: I) -> Res<I, I> {
    peek(pair(
        recognize(tag(":")),
        context("point:version:root_not_trailing", cut(tag("/"))),
    ))(input)
    .map(|(next, (rtn, _))| (next, rtn))
}

// version end of segment
pub fn ver_eos<I: Span>(input: I) -> Res<I, I> {
    peek(alt((fs_trailing, tag(":/"), eop)))(input)
}

// end of point
pub fn eop<I: Span>(input: I) -> Res<I, I> {
    peek(alt((
        eof,
        multispace1,
        tag("<"),
        tag("\""),
        tag("'"),
        tag("]"),
        tag(")"),
        tag("}"),
        tag("^"),
        tag("["),
        tag("("),
        tag("{"),
        tag("%"),
    )))(input)
}

pub fn space_no_dupe_dots<I: Span>(input: I) -> Res<I, ()> {
    context(
        "point:space_segment:dot_dupes",
        peek(cut(not(take_until("..")))),
    )(input)
    .map(|(next, _)| (next, ()))
}

pub fn space_point_segment<I: Span>(input: I) -> Res<I, PointSeg> {
    context(
        "point:space_segment",
        cut(pair(
            recognize(tuple((
                context("point:space_segment_leading", peek(alpha1)),
                space_no_dupe_dots,
                space_chars,
            ))),
            mesh_eos,
        )),
    )(input)
    .map(|(next, (space, x))| (next, PointSeg::Space(space.to_string())))
}

pub fn base_point_segment<I: Span>(input: I) -> Res<I, PointSeg> {
    preceded(
        peek(lowercase1),
        context("point:base_segment", cut(pair(rec_skewer, mesh_eos))),
    )(input)
    .map(|(next, (base, _))| (next, PointSeg::Base(base.to_string())))
}

pub fn version_point_segment<I: Span>(input: I) -> Res<I, PointSeg> {
    preceded(
        peek(digit1),
        context("point:version_segment", cut(tuple((version, ver_eos)))),
    )(input)
    .map(|(next, (version, _))| (next, PointSeg::Version(version)))
}

pub fn dir_pop<I: Span>(input: I) -> Res<I, PointSegVar> {
    context("point:dir_pop", tuple((tag(".."), opt(tag("/")))))(input).map(|(next, _)| {
        (
            next.clone(),
            PointSegVar::Pop(Trace {
                range: next.location_offset() - 2..next.location_offset(),
                extra: next.extra(),
            }),
        )
    })
}

pub fn filesystem_point_segment<I: Span>(input: I) -> Res<I, PointSeg> {
    tuple((
        peek(not(eop)),
        context(
            "point:file_or_directory",
            cut(alt((dir_point_segment, file_point_segment))),
        ),
    ))(input)
    .map(|(next, (_, seg))| (next, seg))
}

pub fn dir_point_segment<I: Span>(input: I) -> Res<I, PointSeg> {
    context("point:dir_segment", file_chars)(input)
        .map(|(next, dir)| (next, PointSeg::Dir(dir.to_string())))
}

pub fn root_dir_point_segment<I: Span>(input: I) -> Res<I, PointSeg> {
    context("point:root_filesystem_segment", tag(":/"))(input)
        .map(|(next, _)| (next, PointSeg::FilesystemRootDir))
}

pub fn root_dir_point_segment_ctx<I: Span>(input: I) -> Res<I, PointSegVar> {
    context("point:root_filesystem_segment", tag(":/"))(input)
        .map(|(next, _)| (next, PointSegVar::FilesystemRootDir))
}

pub fn root_dir_point_segment_var<I: Span>(input: I) -> Res<I, PointSegVar> {
    context("point:root_filesystem_segment", tag(":/"))(input)
        .map(|(next, _)| (next, PointSegVar::FilesystemRootDir))
}

pub fn file_point_segment<I: Span>(input: I) -> Res<I, PointSeg> {
    context("point:file_segment", file_chars)(input)
        .map(|(next, filename)| (next, PointSeg::File(filename.to_string())))
}

pub fn point_var<I: Span>(input: I) -> Res<I, PointVar> {
    context(
        "point",
        tuple((alt((root_point_var, point_non_root_var)), eop)),
    )(input.clone())
    .map(|(next, (point, _))| (next, point))
}

/*
pub fn var<O,F,P>(mut f: F ) -> impl FnMut(I) -> Res<I,Var<O,P>> where F: Parser<I,O,ErrorTree<I>>, P: VarParser<O> {
    move | input: I | {
        let result = recognize(pair(peek(tag("$")),context("var",cut(delimited(tag("${"), skewer_case, tag("}") )))))(input.clone());
        match result {
            Ok((next,var)) => {
                Ok( (next, Var::Var{ name: var.to_string(), parser: f }) )
            }
            Err(err) => {
                match &err {
                    Err::Incomplete(_) => {
                        Err(err)
                    }
                    Err::Failure(_) => {
                        Err(err)
                    }
                    // in this case the peek failed which means it is not a variable declaration
                    Err::Error(_) => {
                        let input = to_owned_span(&input);
                        f.parse(input)
                    }
                }
            }
        }


        let input = to_owned_span(&input);

        input.parse(input)
    }
}

 */

pub fn var_val<F, I: Span, O>(mut f: F) -> impl FnMut(I) -> Res<I, VarVal<O>> + Copy
where
    F: FnMut(I) -> Res<I, O> + Copy,
{
    move |input: I| context("var_val", alt((var, val(f))))(input)
}

fn val<I: Span, O, F>(f: F) -> impl FnMut(I) -> Res<I, VarVal<O>>
where
    F: FnMut(I) -> Res<I, O> + Copy,
{
    move |input| tw(f)(input).map(|(next, val)| (next, VarVal::Val(val)))
}

fn var<I: Span, O>(input: I) -> Res<I, VarVal<O>> {
    tw(|i| delimited(tag("${"), skewer_case, tag("}"))(i))(input)
        .map(|(next, var)| (next, VarVal::Var(var)))
}

pub fn var_seg<F, I: Span>(mut f: F) -> impl FnMut(I) -> Res<I, PointSegVar> + Copy
where
    F: Parser<I, PointSegCtx, ErrorTree<I>> + Copy,
{
    move |input: I| {
        let offset = input.location_offset();
        let result = pair(
            peek(tag("$")),
            context(
                "var",
                cut(delimited(tag("${"), skewer_case_chars, tag("}"))),
            ),
        )(input.clone());

        match result {
            Ok((next, (_, var))) => {
                let range = Range {
                    start: offset,
                    end: next.location_offset(),
                };
                let trace = Trace {
                    range,
                    extra: next.extra(),
                };
                let var = Variable::new(var.to_string(), trace);
                Ok((next, PointSegVar::Var(var)))
            }
            Err(err) => match err {
                Err::Incomplete(needed) => return Err(nom::Err::Incomplete(needed)),
                Err::Failure(err) => return Err(nom::Err::Failure(err)),
                Err::Error(_) => f.parse(input).map(|(next, seg)| (next, seg.into())),
            },
        }
    }
}

pub fn var_route<'a, F, I: Span>(mut f: F) -> impl FnMut(I) -> Res<I, RouteSegVar>
where
    F: Parser<I, RouteSeg, ErrorTree<I>>,
{
    move |input: I| {
        let offset = input.location_offset();
        let result = pair(
            peek(tag("$")),
            context(
                "var",
                cut(delimited(tag("${"), skewer_case_chars, tag("}"))),
            ),
        )(input.clone());
        match result {
            Ok((next, (_, var))) => {
                let range = Range {
                    start: offset,
                    end: next.location_offset(),
                };
                let trace = Trace {
                    range,
                    extra: next.extra(),
                };
                let var = Variable::new(var.to_string(), trace);
                Ok((next, RouteSegVar::Var(var)))
            }
            Err(err) => f.parse(input).map(|(next, seg)| (next, seg.into())),
        }
    }
}
pub fn root_point_var<I: Span>(input: I) -> Res<I, PointVar> {
    context(
        "root_point",
        tuple((
            opt(terminated(var_route(point_route_segment), tag("::"))),
            tag("ROOT"),
        )),
    )(input)
    .map(|(next, (route, _))| {
        let route = route.unwrap_or(RouteSegVar::This);
        let point = PointVar {
            route,
            segments: vec![],
        };
        (next, point)
    })
}

pub fn point_non_root_var<I: Span>(input: I) -> Res<I, PointVar> {
    context(
        "point_non_root",
        tuple((
            context(
                "point_route",
                opt(terminated(var_route(point_route_segment), tag("::"))),
            ),
            var_seg(root_ctx_seg(space_point_segment)),
            many0(base_seg(var_seg(pop(base_point_segment)))),
            opt(base_seg(var_seg(pop(version_point_segment)))),
            opt(tuple((
                root_dir_point_segment_var,
                many0(recognize(tuple((
                    var_seg(pop(dir_point_segment)),
                    tag("/"),
                )))),
                opt(var_seg(pop(file_point_segment))),
                eop,
            ))),
            eop,
        )),
    )(input)
    .map(
        |(next, (route, space, mut bases, version, filesystem, _))| {
            let route = route.unwrap_or(RouteSegVar::This);
            let mut segments = vec![];
            let mut bases: Vec<PointSegVar> = bases;
            segments.push(space);
            segments.append(&mut bases);
            match version {
                None => {}
                Some(version) => {
                    segments.push(version);
                }
            }

            if let Option::Some((fsroot, mut dirs, file, _)) = filesystem {
                let mut dirs: Vec<PointSegVar> = dirs
                    .into_iter()
                    .map(|i| PointSegVar::Dir(i.to_string()))
                    .collect();
                segments.push(fsroot);
                segments.append(&mut dirs);
                if let Some(file) = file {
                    segments.push(file);
                }
            }

            let point = PointVar { route, segments };

            (next, point)
        },
    )
}

pub fn consume_point(input: &str) -> Result<Point, SpaceErr> {
    consume_point_ctx(input)?.collapse()
}

pub fn consume_point_ctx(input: &str) -> Result<PointCtx, SpaceErr> {
    consume_point_var(input)?.collapse()
}

pub fn consume_point_var(input: &str) -> Result<PointVar, SpaceErr> {
    let span = new_span(input);
    let point = result(context("consume", all_consuming(point_var))(span))?;
    Ok(point)
}

/*
pub fn point_old(input: Span) -> Res<Span, Point> {
    let (next, point) = point_subst(input.clone())?;
    match point.brute_resolve() {
        Ok(point) => Ok((next, point)),
        Err(err) => {
            let e = ErrorTree::from_error_kind(input.clone(), ErrorKind::Fail);
            let e = ErrorTree::add_context(input, "point-subst-brute-force", e);
            return Err(nom::Err::Failure(e));
        }
    }
}

 */

/*pub fn capture_point(input: Span) -> Res<Span, CaptureAddress> {
    context(
        "Address",
        tuple((
            tuple((
                point_route_segment,
                alt((root_point_capture_segment, space_point_capture_segment)),
            )),
            many0(base_point_capture_segment),
            opt(version_point_segment),
            opt(root_dir_point_segment),
            many0(filesystem_point_capture_segment),
        )),
    )(input)
    .map(
        |(next, ((hub, space), mut bases, version, root, mut files))| {
            let mut segments = vec![];
            segments.push(space);
            segments.append(&mut bases);
            match version {
                None => {}
                Some(version) => {
                    segments.push(version);
                }
            }

            if let Option::Some(root) = root {
                segments.push(root);
                segments.append(&mut files);
            }

            let point = CaptureAddress {
                route: hub,
                segments,
            };

            (next, point)
        },
    )
}


pub fn root_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    tag("ROOT")(input).map(|(next, space)| (next, PointSeg::Root))
}

pub fn space_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    space_chars_plus_capture(input).map(|(next, space)| (next, PointSeg::Space(space.to_string())))
}

pub fn base_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    preceded(tag(":"), rec_skewer_capture)(input)
        .map(|(next, base)| (next, PointSeg::Base(base.to_string())))
}

pub fn filesystem_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    alt((dir_point_capture_segment, file_point_capture_segment))(input)
}

pub fn dir_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    context(
        "dir_point_capture_segment",
        terminated(file_chars_plus_capture, tag("/")),
    )(input)
    .map(|(next, dir)| (next, PointSeg::Dir(format!("{}/", dir))))
}

pub fn file_point_capture_segment(input: Span) -> Res<Span, PointSeg> {
    context("file_point_capture_segment", file_chars_plus_capture)(input)
        .map(|(next, filename)| (next, PointSeg::File(filename.to_string())))
}
 */



pub fn asterisk<T: Span, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| item.as_char() != '*')
}

pub fn upper<T, E: nom::error::ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| {
        let char_item = item.as_char();

        !char_item.is_uppercase()
    })
}

/*    fn any_resource_path_segment<T>(i: T) -> Res<T, T>
       where
           T: InputTakeAtPosition+nom::InputLength,
           <T as InputTakeAtPosition>::Item: AsChar,
   {
       i.split_at_position1_complete(
           |item| {
               let char_item = item.as_char();
               !(char_item == '-')
                   && !(char_item == '.')
                   && !(char_item == '/')
                   && !(char_item == '_')
                   && !(char_item.is_alpha() || char_item.is_dec_digit())
           },
           ErrorKind::AlphaNumeric,
       )
   }

*/

pub fn in_double_quotes<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item == '\"'
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn skewer_colon<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == ':')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn skewer_dot<I: Span, E>(i: I) -> IResult<I, I, E>
where
    I: InputTakeAtPosition + nom::InputLength,
    <I as InputTakeAtPosition>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn domain<I: Span>(i: I) -> Res<I, Domain> {
    domain_chars(i).map(|(next, domain)| {
        (
            next,
            Domain::new(domain.to_string())
        )
    })
}

pub fn point_segment_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn version_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item != '.'
                && char_item != '-'
                && !char_item.is_digit(10)
                && !(char_item.is_alpha() && char_item.is_lowercase())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn version_req_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '>')
                && !(char_item == '<')
                && !(char_item == '^')
                && !(char_item == '=')
                && !(char_item == '.')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn lowercase1<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item.is_alpha() && char_item.is_lowercase())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn rec_skewer<I: Span>(input: I) -> Res<I, I> {
    recognize(tuple((lowercase1, opt(skewer))))(input)
}

pub fn rec_skewer_capture<I: Span>(input: I) -> Res<I, I> {
    recognize(tuple((lowercase1, opt(skewer_chars_plus_capture))))(input)
}

pub fn camel_chars<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength + Clone + Offset + Slice<RangeTo<usize>>,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    recognize(pair(upper, alphanumeric0))(i)
}

pub fn skewer_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item != '-'
                && !char_item.is_digit(10)
                && !(char_item.is_alpha() && char_item.is_lowercase())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn parse_uuid<I: Span>(i: I) -> Res<I, Uuid> {
    let (next, uuid) = uuid_chars(i.clone())?;
    Ok((
        next,
        Uuid::from(uuid)
            .map_err(|e| nom::Err::Error(ErrorTree::from_error_kind(i, ErrorKind::Tag)))?,
    ))
}

pub fn uuid_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    // same as skewer_chars
    skewer_chars(i)
}

pub fn skewer_chars_plus_capture<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item != '-'
                && char_item != '$'
                && !char_item.is_digit(10)
                && !(char_item.is_alpha() && char_item.is_lowercase())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn skewer_chars_template<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item != '-'
                && char_item.as_char() != '%'
                && !char_item.is_digit(10)
                && !(char_item.is_alpha() && char_item.is_lowercase())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn space_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn space_chars_plus_capture<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '$')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn domain_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn path_regex<I: Span>(input: I) -> Res<I, I> {
    let (next, regex_span) = context("regex", recognize(pair(tag("/"), nospace0)))(input.clone())?;

    let regex_string = regex_span.to_string();
    match Regex::new(regex_string.as_str()) {
        Ok(regex) => Ok((next, regex_span)),
        Err(err) => {
            println!("regex error {}", err.to_string());
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
    }
}

pub fn regex<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '/')
                && !(char_item == ':')
                && !(char_item == '_')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn filepath_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '/')
                && !(char_item == ':')
                && !(char_item == '_')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn file_chars_plus_capture<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '_')
                && !(char_item == '$')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn file_chars<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '_')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn file_chars_template<T: Span>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '.')
                && !(char_item == '_')
                && !(char_item == '%')
                && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn not_space<I: Span>(input: I) -> Res<I, I> {
    is_not(" \n\r\t")(input)
}

pub fn path<I: Span>(input: I) -> Res<I, I> {
    recognize(tuple((tag("/"), opt(filepath_chars))))(input)
}

pub fn subst_path<I: Span>(input: I) -> Res<I, Subst<I>> {
    pair(peek(tag("/")), subst(filepath_chars))(input).map(|(next, (_, path))| (next, path))
}

pub fn consume_path<I: Span>(input: I) -> Res<I, I> {
    all_consuming(path)(input)
}

pub fn camel_case<I: Span>(input: I) -> Res<I, CamelCase> {
    context("expect-camel-case", camel_case_chars)(input).map(|(next, camel_case_chars)| {
        (
            next,
            CamelCase::new(
                 camel_case_chars.to_string())
        )
    })
}

pub fn skewer_case<I: Span>(input: I) -> Res<I, SkewerCase> {
    context("expect-skewer-case", skewer_case_chars)(input).map(|(next, skewer_case_chars)| {
        (
            next,
            SkewerCase::new(
                 skewer_case_chars.to_string()
            ),
        )
    })
}

pub fn camel_case_chars<I: Span>(input: I) -> Res<I, I> {
    recognize(tuple((is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alphanumeric0)))(input)
}

pub fn skewer_case_chars<I: Span>(input: I) -> Res<I, I> {
    recognize(tuple((
        is_a("abcdefghijklmnopqrstuvwxyz"),
        many0(alt((alphanumeric1, tag("-")))),
    )))(input)
}

pub fn lowercase_alphanumeric<I: Span>(input: I) -> Res<I, I> {
    recognize(tuple((lowercase1, alphanumeric0)))(input)
}

pub fn single_lowercase<T: Span, Input, Error: ParseError<Input>>(
    arr: T,
) -> impl Fn(Input) -> IResult<Input, Input, Error>
where
    Input: InputTakeAtPosition,
    T: FindToken<<Input as InputTakeAtPosition>::Item>,
{
    move |i: Input| {
        let e: ErrorKind = ErrorKind::IsA;
        i.split_at_position1_complete(|c| !arr.find_token(c), e)
    }
}

pub fn single_lowerscase<I: Span>(input: I) -> Res<I, I> {
    is_a("abcdefghijklmnopqrstuvwxyz")(input)
}
pub fn single_digit<I: Span>(input: I) -> Res<I, I> {
    is_a("abcdefghijklmnopqrstuvwxyz")(input)
}

pub fn camel_case_to_string_matcher<I: Span>(input: I) -> Res<I, StringMatcher> {
    camel_case_chars(input).map(|(next, camel)| (next, StringMatcher::new(camel.to_string())))
}

fn parse_version_major_minor_patch<I: Span>(input: I) -> Res<I, (I, I, I)> {
    context(
        "version_major_minor_patch",
        tuple((
            terminated(digit1, tag(".")),
            terminated(digit1, tag(".")),
            terminated(digit1, not(digit1)),
        )),
    )(input)
}

pub fn parse_version<I: Span>(input: I) -> Res<I, ((I, I, I), Option<I>)> {
    tuple((
        parse_version_major_minor_patch,
        opt(preceded(tag("-"), skewer_chars)),
    ))(input)
}

pub fn rec_version<I: Span>(input: I) -> Res<I, I> {
    recognize(parse_version)(input)
}

pub fn base_point_segment_wildcard<I: Span>(input: I) -> Res<I, PointTemplateSeg> {
    preceded(
        tag(":"),
        recognize(tuple((many0(skewer), tag("%"), many0(skewer)))),
    )(input)
    .map(|(next, base)| (next, PointTemplateSeg::Wildcard(base.to_string())))
}

pub fn base_point_segment_template<I: Span>(input: I) -> Res<I, PointTemplateSeg> {
    preceded(tag(":"), rec_skewer)(input).map(|(next, base)| {
        (
            next,
            PointTemplateSeg::ExactSeg(PointSeg::Base(base.to_string())),
        )
    })
}

pub fn filepath_point_segment_wildcard<I: Span>(input: I) -> Res<I, PointTemplateSeg> {
    recognize(tuple((
        many0(filepath_chars),
        tag("%"),
        many0(filepath_chars),
    )))(input)
    .map(|(next, base)| (next, PointTemplateSeg::Wildcard(base.to_string())))
}

pub fn filepath_point_segment_template<I: Span>(input: I) -> Res<I, PointTemplateSeg> {
    filesystem_point_segment(input)
        .map(|(next, segment)| (next, PointTemplateSeg::ExactSeg(segment)))
}

/*pub fn point_template<I: Span>(input: I) -> Res<I, PointTemplate> {
    let (next, ((hub, space), mut bases, version, root, mut files)) = tuple((
        tuple((point_route_segment, space_point_segment)),
        many0(alt((
            base_point_segment_wildcard,
            base_point_segment_template,
        ))),
        opt(version_point_segment),
        opt(root_dir_point_segment),
        many0(alt((
            filepath_point_segment_wildcard,
            filepath_point_segment_template,
        ))),
    ))(input.clone())?;

    let mut base_wildcard = false;
    for (index, segment) in bases.iter().enumerate() {
        if segment.is_wildcard() {
            if index != bases.len() - 1 {
                return Err(nom::Err::Error(ErrorTree::from_error_kind(
                    input,
                    ErrorKind::Tag,
                )));
            } else {
                base_wildcard = true;
            }
        }
    }

    if base_wildcard && version.is_some() {
        return Err(nom::Err::Error(ErrorTree::from_error_kind(
            input,
            ErrorKind::Tag,
        )));
    }

    if base_wildcard && root.is_some() {
        return Err(nom::Err::Error(ErrorTree::from_error_kind(
            input,
            ErrorKind::Tag,
        )));
    }

    let mut files_wildcard = false;
    for (index, segment) in files.iter().enumerate() {
        if segment.is_wildcard() {
            if index != files.len() - 1 {
                return Err(nom::Err::Error(ErrorTree::from_error_kind(
                    input.clone(),
                    ErrorKind::Tag,
                )));
            } else {
                files_wildcard = true;
            }
        }
    }

    let mut space_last = false;
    let last = if !files.is_empty() {
        match files.remove(files.len() - 1) {
            PointTemplateSeg::ExactSeg(exact) => PointSegFactory::Exact(exact.to_string()),
            PointTemplateSeg::Wildcard(pattern) => PointSegFactory::Pattern(pattern),
        }
    } else if root.is_some() {
        PointSegFactory::Exact("/".to_string())
    } else if let Option::Some(version) = &version {
        PointSegFactory::Exact(version.to_string())
    } else if !bases.is_empty() {
        match bases.remove(bases.len() - 1) {
            PointTemplateSeg::ExactSeg(exact) => PointSegFactory::Exact(exact.to_string()),
            PointTemplateSeg::Wildcard(pattern) => PointSegFactory::Pattern(pattern),
        }
    } else {
        space_last = true;
        PointSegFactory::Exact(space.to_string())
    };

    let mut bases: Vec<PointSeg> = bases
        .into_iter()
        .map(|b| match b {
            PointTemplateSeg::ExactSeg(seg) => seg,
            PointTemplateSeg::Wildcard(_) => {
                panic!("should have filtered wildcards already!")
            }
        })
        .collect();

    let mut files: Vec<PointSeg> = files
        .into_iter()
        .map(|b| match b {
            PointTemplateSeg::ExactSeg(seg) => seg,
            PointTemplateSeg::Wildcard(_) => {
                panic!("should have filtered wildcards already!")
            }
        })
        .collect();

    let mut segments = vec![];

    if !space_last {
        segments.push(space);
    }

    segments.append(&mut bases);

    match version {
        None => {}
        Some(version) => {
            segments.push(version);
        }
    }

    if let Option::Some(root) = root {
        segments.push(root);
        segments.append(&mut files);
    }

    let point = Point {
        route: hub,
        segments,
    };

    let point_template = PointTemplate {
        parent: point,
        child_segment_template: last,
    };

    Ok((next, point_template))
}

 */

pub fn point_template<I: Span>(input: I) -> Res<I, PointTemplateVar> {
    let (next, (point, wildcard)) = pair(point_var, opt(recognize(tag("%"))))(input.clone())?;

    if point.is_root() {
        return Ok((
            next,
            PointTemplateVar {
                parent: point,
                child_segment_template: PointSegTemplate::Root,
            },
        ));
    }

    let parent = point
        .parent()
        .expect("expect that point template has a parent");
    let child = point
        .last_segment()
        .expect("expect that point template has a last segment");

    match wildcard {
        None => Ok((
            next,
            PointTemplateVar {
                parent,
                child_segment_template: PointSegTemplate::Exact(child.to_string()),
            },
        )),
        Some(_) => {
            let child = format!("{}%", child.to_string());
            Ok((
                next,
                PointTemplateVar {
                    parent,
                    child_segment_template: PointSegTemplate::Exact(child),
                },
            ))
        }
    }
}


pub fn template<I: Span>(input: I) -> Res<I, TemplateVar> {
    tuple((point_template, delimited(tag("<"), kind_selector, tag(">"))))(input)
        .map(|(next, (point, kind))| (next, TemplateVar { point, kind }))
}

pub fn set_property_mod<I: Span>(input: I) -> Res<I, PropertyMod> {
    tuple((tag("+"), skewer_dot, tag("="), property_value))(input).map(
        |(next, (_, key, _, value))| {
            (
                next,
                PropertyMod::Set {
                    key: key.to_string(),
                    value: value.to_string(),
                    lock: false,
                },
            )
        },
    )
}

pub fn set_property_mod_lock<I: Span>(input: I) -> Res<I, PropertyMod> {
    tuple((tag("+@"), skewer_dot, tag("="), property_value))(input).map(
        |(next, (_, key, _, value))| {
            (
                next,
                PropertyMod::Set {
                    key: key.to_string(),
                    value: value.to_string(),
                    lock: true,
                },
            )
        },
    )
}

pub fn property_value_not_space_or_comma<I: Span>(input: I) -> Res<I, I> {
    is_not(" \n\r\t,")(input)
}

pub fn property_value_single_quotes<I: Span>(input: I) -> Res<I, I> {
    delimited(tag("'"), is_not("'"), tag("'"))(input)
}

pub fn property_value_double_quotes<I: Span>(input: I) -> Res<I, I> {
    delimited(tag("\""), is_not("\""), tag("\""))(input)
}

pub fn property_value<I: Span>(input: I) -> Res<I, I> {
    alt((
        property_value_single_quotes,
        property_value_double_quotes,
        property_value_not_space_or_comma,
    ))(input)
}

pub fn unset_property_mod<I: Span>(input: I) -> Res<I, PropertyMod> {
    tuple((tag("!"), skewer_dot))(input)
        .map(|(next, (_, name))| (next, PropertyMod::UnSet(name.to_string())))
}

pub fn property_mod<I: Span>(input: I) -> Res<I, PropertyMod> {
    alt((set_property_mod, unset_property_mod))(input)
}

pub fn set_properties<I: Span>(input: I) -> Res<I, SetProperties> {
    separated_list0(tag(","), tuple((multispace0, property_mod, multispace0)))(input).map(
        |(next, properties)| {
            let mut set_properties = SetProperties::new();
            for (_, property, _) in properties {
                set_properties.push(property);
            }
            (next, set_properties)
        },
    )
}

pub fn get_properties<I: Span>(input: I) -> Res<I, Vec<String>> {
    separated_list0(tag(","), tuple((multispace0, skewer, multispace0)))(input).map(
        |(next, keys)| {
            let keys: Vec<String> = keys.iter().map(|(_, key, _)| key.to_string()).collect();
            (next, keys)
        },
    )
}

pub fn create<I: Span>(input: I) -> Res<I, CreateVar> {
    tuple((
        opt(alt((
            value(Strategy::Override, tag("!")),
            value(Strategy::Ensure, tag("?")),
        ))),
        space1,
        template,
        opt(delimited(tag("{"), set_properties, tag("}"))),
    ))(input)
    .map(|(next, (strategy, _, template, properties))| {
        let strategy = match strategy {
            None => Strategy::Commit,
            Some(strategy) => strategy,
        };
        let properties = match properties {
            Some(properties) => properties,
            None => SetProperties::new(),
        };
        let create = CreateVar {
            template,
            state: StateSrcVar::None,
            properties,
            strategy,
        };
        (next, create)
    })
}

pub fn set<I: Span>(input: I) -> Res<I, SetVar> {
    tuple((point_var, delimited(tag("{"), set_properties, tag("}"))))(input).map(
        |(next, (point, properties))| {
            let set = SetVar { point, properties };
            (next, set)
        },
    )
}

pub fn get<I: Span>(input: I) -> Res<I, GetVar> {
    tuple((
        point_var,
        opt(delimited(tag("{"), get_properties, tag("}"))),
    ))(input)
    .map(|(next, (point, keys))| {
        let op = match keys {
            None => GetOp::State,
            Some(keys) => GetOp::Properties(keys),
        };
        let get = GetVar { point, op };

        (next, get)
    })
}

pub fn select<I: Span>(input: I) -> Res<I, SelectVar> {
    point_selector_var(input).map(|(next, point_kind_pattern)| {
        let select = SelectVar {
            pattern: point_kind_pattern,
            properties: Default::default(),
            into_substance: SelectIntoSubstance::Stubs,
        };
        (next, select)
    })
}

pub fn publish<I: Span>(input: I) -> Res<I, CreateVar> {
    let (next, (upload, _, point)) = tuple((upload_block, space1, point_template))(input.clone())?;

    /*
    let parent = match point.parent() {
        None => {
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
        Some(parent) => parent,
    };

    let last = match point.last_segment() {
        None => {
            return Err(nom::Err::Error(ErrorTree::from_error_kind(
                input.clone(),
                ErrorKind::Tag,
            )));
        }
        Some(last) => last,
    };
     */

    todo!();
    /*
    let template = TemplateVar {
        point,
        kind: KindTemplate {
            base: KindCat::Bundle,
            sub: None,
            specific: None,
        },
    };

    let create = CreateVar {
        template,
        state: StateSrcVar::None,
        properties: Default::default(),
        strategy: Strategy::Commit,
    };


    Ok((next, create))

     */
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Ctx {
    WorkingPoint,
    PointFromRoot,
}

impl ToString for Ctx {
    fn to_string(&self) -> String {
        match self {
            Ctx::WorkingPoint => ".".to_string(),
            Ctx::PointFromRoot => "...".to_string(),
        }
    }
}

/*
#[derive(Clone)]
pub struct Env {
    point: Option<Point>,
    var_resolver: Option<CompositeResolver>,
    file_resolver: Option<FileResolver>,
}

impl Env {
    pub fn add_var_resolver(&mut self, resolver: Arc<dyn VarResolver>) {
        if let Some(r) = self.var_resolver.as_mut() {
            r.other_resolver.push(resolver)
        }
    }

    pub fn no_point() -> Self {
        Self {
            point: None,
            var_resolver: Some(CompositeResolver::new()),
            file_resolver: None,
        }
    }

    pub fn unavailable() -> Self {
        Self {
            point: None,
            var_resolver: None,
            file_resolver: None,
        }
    }

    pub fn new(point: Point) -> Self {
        Self {
            point: Some(point),
            var_resolver: Some(CompositeResolver::new()),
            file_resolver: Some(FileResolver::new()),
        }
    }

    pub fn just_point(point: Point) -> Self {
        Self {
            point: Some(point),
            var_resolver: None,
            file_resolver: None,
        }
    }

    pub fn point_or(&self) -> Result<&Point, ExtErr> {
        self.point
            .as_ref()
            .ok_or("cannot reference working point in this context".into())
    }

    pub fn val(&self, var: &str) -> Result<String, ResolverErr> {
        if let None = self.var_resolver {
            Err(ResolverErr::NotAvailable)
        } else {
            self.var_resolver.as_ref().unwrap().val(var)
        }
    }

    pub fn set_working(&mut self, point: Point) {
        self.point.replace(point);
    }

    pub fn set_var<V: ToString>(&mut self, key: V, value: V) {
        match self.var_resolver.as_mut() {
            None => {
                let mut var_resolver = CompositeResolver::new();
                var_resolver.set(key, value);
                self.var_resolver.replace(var_resolver);
            }
            Some(var_resolver) => {
                var_resolver.set(key, value);
            }
        }
    }

    pub fn file<N: ToString>(&self, name: N) -> Result<File, ResolverErr> {
        match &self.file_resolver {
            None => Err(ResolverErr::NotAvailable),
            Some(file_resolver) => file_resolver.file(name),
        }
    }

    pub fn set_file<N: ToString>(&mut self, name: N, content: Bin) {
        match self.file_resolver.as_mut() {
            None => {
                let mut file_resolver = FileResolver::new();
                file_resolver.files.insert(name.to_string(), content);
                self.file_resolver.replace(file_resolver);
            }
            Some(file_resolver) => {
                file_resolver.files.insert(name.to_string(), content);
            }
        }
    }
}

 */

pub trait CtxResolver {
    fn working_point(&self) -> Result<&Point, SpaceErr>;
}

pub enum ResolverErr {
    NotAvailable,
    NotFound,
}

pub trait VarResolver: Send + Sync {
    fn val(&self, var: &str) -> Result<Substance, ResolverErr> {
        Err(ResolverErr::NotFound)
    }
}

/*
pub trait BruteResolver<Resolved>
where
    Self: Sized + ToResolved<Resolved>,
{
    fn brute_resolve(self) -> Result<Resolved, ExtErr> {
        let resolver = NoResolver::new().wrap();
        Ok(self.to_resolved(&resolver)?)
    }
}

 */

pub fn diagnose<I: Clone, O, E: ParseError<I>, F>(
    tag: &'static str,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    F: nom::Parser<I, O, E>,
    E: nom::error::ContextError<I>,
    O: Clone,
{
    move |input: I| {
        let (next, i) = f.parse(input)?;
        Ok((next, i))
    }
}

pub trait SubstParser<T: Sized> {
    fn parse_string(&self, string: String) -> Result<T, SpaceErr> {
        let span = new_span(string.as_str());
        let output = result(self.parse_span(span))?;
        Ok(output)
    }

    fn parse_span<I: Span>(&self, input: I) -> Res<I, T>;
}

pub fn root_ctx_seg<I: Span, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, PointSegCtx, E> + Copy
where
    F: nom::Parser<I, PointSeg, E> + Copy,
    E: nom::error::ContextError<I>,
{
    move |input: I| match pair(tag::<&str, I, E>(".."), eos)(input.clone()) {
        Ok((next, v)) => Ok((
            next.clone(),
            PointSegCtx::Pop(Trace {
                range: next.location_offset() - 2..next.location_offset(),
                extra: next.extra(),
            }),
        )),
        Err(err) => match pair(tag::<&str, I, E>("."), eos)(input.clone()) {
            Ok((next, _)) => Ok((
                next.clone(),
                PointSegCtx::Working(Trace {
                    range: next.location_offset() - 1..next.location_offset(),
                    extra: next.extra(),
                }),
            )),
            Err(err) => match f.parse(input) {
                Ok((next, seg)) => Ok((next, seg.into())),
                Err(err) => Err(err),
            },
        },
    }
}

pub fn working<I: Span, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, PointSegCtx, E>
where
    F: nom::Parser<I, PointSeg, E>,
    E: nom::error::ContextError<I>,
{
    move |input: I| match pair(tag::<&str, I, E>("."), eos)(input.clone()) {
        Ok((next, v)) => Ok((
            next.clone(),
            PointSegCtx::Working(Trace {
                range: next.location_offset() - 1..next.location_offset(),
                extra: next.extra(),
            }),
        )),
        Err(err) => match f.parse(input.clone()) {
            Ok((next, seg)) => Ok((next, seg.into())),
            Err(err) => Err(err),
        },
    }
}

pub fn pop<I: Span, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, PointSegCtx, E> + Copy
where
    F: nom::Parser<I, PointSeg, E> + Copy,
    E: nom::error::ContextError<I>,
{
    move |input: I| match pair(tag::<&str, I, E>(".."), eos)(input.clone()) {
        Ok((next, v)) => Ok((
            next.clone(),
            PointSegCtx::Working(Trace {
                range: next.location_offset() - 2..next.location_offset(),
                extra: next.extra(),
            }),
        )),
        Err(err) => match f.parse(input.clone()) {
            Ok((next, seg)) => Ok((next, seg.into())),
            Err(err) => Err(err),
        },
    }
}

pub fn base_seg<I, F, S, E>(mut f: F) -> impl FnMut(I) -> IResult<I, S, E>
where
    I: Span,
    F: nom::Parser<I, S, E> + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
    S: PointSegment,
{
    move |input: I| preceded(tag(":"), f)(input)
}

pub fn mesh_seg<I: Span, E: ParseError<I>, F, S1, S2>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, S2, E>
where
    F: nom::Parser<I, S1, E> + Copy,
    E: nom::error::ContextError<I>,
    S1: PointSegment + Into<S2>,
    S2: PointSegment,
{
    move |input: I| {
        tuple((seg_delim, f, eos))(input).map(|(next, (delim, seg, _))| (next, seg.into()))
    }
}

// end of segment
pub fn seg_delim<I: Span, E>(input: I) -> IResult<I, PointSegDelim, E>
where
    I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    alt((
        value(PointSegDelim::File, tag("/")),
        value(PointSegDelim::Mesh, tag(":")),
    ))(input)
    .map(|(next, delim)| (next, delim))
}

// end of segment
pub fn eos<I: Span, E>(input: I) -> IResult<I, (), E>
where
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    peek(alt((tag("/"), tag(":"), tag("%"), space1, eof)))(input).map(|(next, _)| (next, ()))
}

/*
pub fn var<O>(f: impl Fn(Span) -> Res<Span,O>+'static+Clone) -> impl FnMut(Span) -> Res<Span, Symbol<O>>
{
    unimplemented!()
    /*
    move |input: Span| {
        preceded(
            tag("$"),
            context(
                "variable",
                cut(delimited(
                    context("variable:open", tag("(")),
                    variable_name,
                    tag(")"),
                )),
            ),
        )(input)
        .map(|(next, name)| {
            (
                next,
                Symbol::named(name.to_string(), f.clone() ),
            )
        })
    }

     */
}

 */

/*jjj
pub fn ctx<O><I:Span>(input: Span) -> Res<Span, Symbol<O>>

{
    alt((
        value(Ctx::RelativePointPop, tuple((tag(".."), eos))),
        value(Ctx::RelativePoint, tuple((tag("."), eos))),
    ))(input)
        .map(|(next, ctx)| (next, Symbol::ctx(ctx)))
}

 */

/*
pub fn ctx<I, O, F, E><I:Span>(input: I) -> Res<I, Symbol<O>>
where
    I: ToString
        + Clone
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: nom::Parser<I, O, E> + Clone,
    E: nom::error::ContextError<I>,
{
    alt((
        value(Ctx::RelativePointPop, tuple((tag(".."), eos))),
        value(Ctx::RelativePoint, tuple((tag("."), eos))),
    ))(input)
    .map(|(next, ctx)| (next, Symbol::ctx(ctx)))
}

 */

pub fn variable_name<I: Span>(input: I) -> Res<I, I> {
    recognize(pair(lowercase1, opt(skewer_dot)))(input).map(|(next, name)| (next, name))
}

pub fn ispan<'a, I: Clone, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Spanned<I, O>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    F: nom::Parser<I, O, E>,
    E: nom::error::ContextError<I>,
    O: Clone + FromStr<Err = SpaceErr>,
{
    move |input: I| {
        let (next, element) = f.parse(input.clone())?;
        Ok((next, Spanned::new(element, input.clone())))
    }
}

pub fn sub<I: Span, O, F>(mut f: F) -> impl FnMut(I) -> Res<I, Spanned<I, O>>
where
    F: nom::Parser<I, O, ErrorTree<I>>,
    O: Clone,
{
    move |input: I| {
        let (next, element) = f.parse(input.clone())?;
        Ok((
            next.clone(),
            Spanned::new(element, input.slice(0..(input.len() - next.len()))),
        ))
    }
}

pub fn access_grant_kind<I: Span>(input: I) -> Res<I, AccessGrantKind> {
    tuple((
        context(
            "access_grant_kind",
            peek(alt((
                tuple((tag("perm"), space1)),
                tuple((tag("priv"), space1)),
            ))),
        ),
        alt((access_grant_kind_perm, access_grant_kind_priv)),
    ))(input)
    .map(|(next, (_, kind))| (next, kind))
}

pub fn access_grant_kind_priv<I: Span>(input: I) -> Res<I, AccessGrantKind> {
    tuple((
        tag("priv"),
        context("access_grant:priv", tuple((space1, privilege))),
    ))(input)
    .map(|(next, (_, (_, privilege)))| (next, AccessGrantKindDef::Privilege(privilege)))
}

pub fn access_grant_kind_perm<I: Span>(input: I) -> Res<I, AccessGrantKind> {
    tuple((
        tag("perm"),
        context("access_grant:perm", tuple((space1, permissions_mask))),
    ))(input)
    .map(|(next, (_, (_, perms)))| (next, AccessGrantKindDef::PermissionsMask(perms)))
}

pub fn privilege<I: Span>(input: I) -> Res<I, Privilege> {
    context("privilege", alt((tag("*"), skewer_colon)))(input).map(|(next, prv)| {
        let prv = match prv.to_string().as_str() {
            "*" => Privilege::Full,
            prv => Privilege::Single(prv.to_string()),
        };
        (next, prv)
    })
}

pub fn permissions_mask<I: Span>(input: I) -> Res<I, PermissionsMask> {
    context(
        "permissions_mask",
        tuple((
            alt((
                value(PermissionsMaskKind::Or, char('+')),
                value(PermissionsMaskKind::And, char('&')),
            )),
            permissions,
        )),
    )(input)
    .map(|(next, (kind, permissions))| {
        let mask = PermissionsMask { kind, permissions };

        (next, mask)
    })
}

pub fn permissions<I: Span>(input: I) -> Res<I, Permissions> {
    context(
        "permissions",
        tuple((child_perms, tag("-"), particle_perms)),
    )(input)
    .map(|(next, (child, _, particle))| {
        let permissions = Permissions { child, particle };
        (next, permissions)
    })
}

pub fn child_perms<I: Span>(input: I) -> Res<I, ChildPerms> {
    context(
        "child_perms",
        alt((
            tuple((
                alt((value(false, char('c')), value(true, char('C')))),
                alt((value(false, char('s')), value(true, char('S')))),
                alt((value(false, char('d')), value(true, char('D')))),
            )),
            fail,
        )),
    )(input)
    .map(|(next, (create, select, delete))| {
        let block = ChildPerms {
            create,
            select,
            delete,
        };
        (next, block)
    })
}

pub fn particle_perms<I: Span>(input: I) -> Res<I, ParticlePerms> {
    context(
        "particle_perms",
        tuple((
            alt((value(false, char('r')), value(true, char('R')))),
            alt((value(false, char('w')), value(true, char('W')))),
            alt((value(false, char('x')), value(true, char('X')))),
        )),
    )(input)
    .map(|(next, (read, write, execute))| {
        let block = ParticlePerms {
            read,
            write,
            execute,
        };
        (next, block)
    })
}

/*
pub fn grant<I><I:Span>(input: I) -> Res<I,AccessGrant> where I:Clone+InputIter+InputLength+InputTake{

}

 */

pub fn none<I: Span, O, E>(input: I) -> IResult<I, Option<O>, E> {
    Ok((input, None))
}

pub fn some<I: Span, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Option<O>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
    F: nom::Parser<I, O, E> + Clone,
{
    move |input: I| {
        f.clone()
            .parse(input)
            .map(|(next, output)| (next, Some(output)))
    }
}

pub fn lex_block_alt<I: Span, E>(
    kinds: Vec<BlockKind>,
) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        for kind in &kinds {
            let result = lex_block(kind.clone())(input.clone());
            match &result {
                Ok((next, block)) => return result,
                Err(err) => {
                    match err {
                        nom::Err::Incomplete(Needed) => return result,
                        nom::Err::Error(e) => {
                            // let's hope for another match...
                        }
                        nom::Err::Failure(e) => return result,
                    }
                }
            }
        }

        Err(nom::Err::Failure(E::from_error_kind(
            input.clone(),
            ErrorKind::Alt,
        )))
    }
}

pub fn lex_block<I: Span, E>(kind: BlockKind) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| match kind {
        BlockKind::Nested(kind) => lex_nested_block(kind).parse(input),
        BlockKind::Terminated(kind) => lex_terminated_block(kind).parse(input),
        BlockKind::Delimited(kind) => lex_delimited_block(kind).parse(input),
        BlockKind::Partial => {
            eprintln!("parser should not be seeking partial block kinds...");
            Err(nom::Err::Failure(E::from_error_kind(
                input,
                ErrorKind::IsNot,
            )))
        }
    }
}

pub fn lex_terminated_block<I: Span, E>(
    kind: TerminatedBlockKind,
) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        terminated(
            recognize(many0(satisfy(|c| c != kind.as_char()))),
            tag(kind.tag()),
        )(input)
        .map(|(next, content)| {
            let block = LexBlock {
                kind: BlockKind::Terminated(kind),
                content,
                data: (),
            };

            (next, block)
        })
    }
}

/// rough block simply makes sure that the opening and closing symbols match
/// it accounts for multiple embedded blocks of the same kind but NOT of differing kinds
pub fn lex_nested_block<I: Span, E>(
    kind: NestedBlockKind,
) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        let (next, content) = context(
            kind.context(),
            delimited(
                context(kind.open_context(), tag(kind.open())),
                recognize(many0(alt((
                    recognize(lex_nested_block(kind.clone())),
                    recognize(tuple((
                        not(peek(tag(kind.close()))),
                        alt((recognize(pair(tag("\\"), anychar)), recognize(anychar))),
                    ))),
                )))),
                context(kind.close_context(), cut(tag(kind.close()))),
            ),
        )(input)?;
        let block = Block::parse(BlockKind::Nested(kind), content);
        Ok((next, block))
    }
}

pub fn nested_block_content<I: Span>(kind: NestedBlockKind) -> impl FnMut(I) -> Res<I, I> {
    move |input: I| nested_block(kind)(input).map(|(next, block)| (next, block.content))
}

pub fn nested_block<I: Span>(kind: NestedBlockKind) -> impl FnMut(I) -> Res<I, Block<I, ()>> {
    move |input: I| {
        let (next, content) = context(
            kind.context(),
            delimited(
                context(kind.open_context(), tag(kind.open())),
                recognize(many0(tuple((
                    not(peek(tag(kind.close()))),
                    context(
                        kind.unpaired_closing_scope(),
                        cut(peek(expected_block_terminator_or_non_terminator(
                            kind.clone(),
                        ))),
                    ),
                    alt((
                        recognize(pair(peek(block_open), any_block)),
                        recognize(anychar),
                    )),
                )))),
                context(kind.close_context(), cut(tag(kind.close()))),
            ),
        )(input)?;
        let block = Block::parse(BlockKind::Nested(kind), content);
        Ok((next, block))
    }
}

pub fn lex_delimited_block<I: Span, E>(
    kind: DelimitedBlockKind,
) -> impl FnMut(I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    move |input: I| {
        let (next, content) = context(
            kind.context(),
            delimited(
                context(kind.context(), tag(kind.delim())),
                recognize(many0(tuple((
                    not(peek(tag(kind.delim()))),
                    alt((recognize(pair(tag("\\"), anychar)), recognize(anychar))),
                )))),
                context(kind.missing_close_context(), cut(tag(kind.delim()))),
            ),
        )(input)?;
        let block = Block::parse(BlockKind::Delimited(kind), content);
        Ok((next, block))
    }
}

fn block_open<I: Span>(input: I) -> Res<I, NestedBlockKind> {
    alt((
        value(NestedBlockKind::Curly, tag(NestedBlockKind::Curly.open())),
        value(NestedBlockKind::Angle, tag(NestedBlockKind::Angle.open())),
        value(NestedBlockKind::Parens, tag(NestedBlockKind::Parens.open())),
        value(NestedBlockKind::Square, tag(NestedBlockKind::Square.open())),
    ))(input)
}

fn any_soround_lex_block<I: Span, E>(input: I) -> IResult<I, LexBlock<I>, E>
where
    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
{
    alt((
        lex_nested_block(NestedBlockKind::Curly),
        lex_nested_block(NestedBlockKind::Angle),
        lex_nested_block(NestedBlockKind::Parens),
        lex_nested_block(NestedBlockKind::Square),
        lex_delimited_block(DelimitedBlockKind::DoubleQuotes),
        lex_delimited_block(DelimitedBlockKind::SingleQuotes),
    ))(input)
}

fn any_block<I: Span>(input: I) -> Res<I, LexBlock<I>> {
    alt((
        nested_block(NestedBlockKind::Curly),
        nested_block(NestedBlockKind::Angle),
        nested_block(NestedBlockKind::Parens),
        nested_block(NestedBlockKind::Square),
        lex_delimited_block(DelimitedBlockKind::DoubleQuotes),
        lex_delimited_block(DelimitedBlockKind::SingleQuotes),
    ))(input)
}

pub fn expected_block_terminator_or_non_terminator<I: Span>(
    expect: NestedBlockKind,
) -> impl FnMut(I) -> Res<I, ()>
where
    I: InputIter + InputLength + Slice<RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    I: Clone,
{
    move |input: I| -> Res<I, ()> {
        verify(anychar, move |c| {
            if NestedBlockKind::is_block_terminator(*c) {
                *c == expect.close_as_char()
            } else {
                true
            }
        })(input)
        .map(|(next, _)| (next, ()))
    }
}

/*
pub fn lex_hierarchy_scope<'a>(
    scope: LexScope<Span<'a>>,
    max_depth: usize,
) -> Result<LexHierarchyScope<'a>, ExtErr> {
    let mut errs = vec![];
    let scope = lex_child_scopes(scope)?;
    let mut children = vec![];

    for child in scope.block {
        if max_depth <= 0 {
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message("exceeded max depth hierarchy for nested scopes")
                .with_label(
                    Label::new(
                        child.block.content.location_offset()
                            ..child.block.content.location_offset() + child.block.content.len(),
                    )
                    .with_message("Nest Limit Exceeded"),
                )
                .finish();
            return Err(ParseErrs::new(report, child.block.content.extra.clone()).into());
        }
        match lex_hierarchy_scope(child, max_depth - 1) {
            Ok(child) => {
                children.push(child);
            }
            Err(err) => errs.push(err),
        }
    }

    Ok(LexHierarchyScope::new(scope.selector.clone(), children))
}*/

pub fn unwrap_block<I: Span, F, O>(kind: BlockKind, mut f: F) -> impl FnMut(I) -> Res<I, O>
where
    F: FnMut(I) -> Res<I, O>,
{
    move |input: I| {
        let (next, block) = lex_block(kind)(input)?;
        let (_, content) = f.parse(block.content)?;
        //        let (_, content) = context("block", f)(block.content)?;
        Ok((next, content))
    }
}

pub fn lex_child_scopes<I: Span>(parent: LexScope<I>) -> Result<LexParentScope<I>, SpaceErr> {
    if parent.selector.children.is_some() {
        let (_, child_selector) = all_consuming(lex_scope_selector)(
            parent
                .selector
                .children
                .as_ref()
                .expect("child names...")
                .clone(),
        )?;

        let child = LexScope::new(child_selector.into(), parent.block);

        Ok(LexParentScope {
            selector: parent.selector.clone(),
            pipeline_step: None,
            block: vec![child],
        })
    } else {
        let scopes = lex_scopes(parent.block.content)?;

        Ok(LexParentScope {
            selector: parent.selector.into(),
            pipeline_step: parent.pipeline_step,
            block: scopes,
        })
    }
}

pub fn lex_scope<I: Span>(input: I) -> Res<I, LexScope<I>> {
    context(
        "scope",
        tuple((
            peek(alt((tag("*"), alpha1, tag("<")))),
            lex_scope_selector,
            multispace1,
            lex_scope_pipeline_step_and_block,
        )),
    )(input)
    .map(|(next, (_, selector, _, (pipeline_step, block)))| {
        let scope = LexScope {
            selector,
            pipeline_step,
            block,
        };
        (next, scope)
    })
}

pub fn lex_scoped_block_kind<I: Span>(input: I) -> Res<I, BlockKind> {
    alt((
        value(
            BlockKind::Nested(NestedBlockKind::Curly),
            recognize(tuple((
                multispace0,
                rough_pipeline_step,
                multispace0,
                lex_block(BlockKind::Nested(NestedBlockKind::Curly)),
            ))),
        ),
        value(
            BlockKind::Terminated(TerminatedBlockKind::Semicolon),
            recognize(pair(
                rough_pipeline_step,
                lex_block(BlockKind::Terminated(TerminatedBlockKind::Semicolon)),
            )),
        ),
    ))(input)
}

pub fn rough_pipeline_step<I:Span> ( input: I ) -> Res<I,Pipeline> {
    todo!()
}

pub fn lex_scope_pipeline_step_and_block<I: Span>(input: I) -> Res<I, (Option<I>, LexBlock<I>)> {
  todo!()
}

pub fn lex_sub_scope_selectors_and_filters_and_block<I: Span>(input: I) -> Res<I, LexBlock<I>> {
    recognize(pair(
        nested_block_content(NestedBlockKind::Angle),
        tuple((
            opt(scope_filters),
            multispace0,
            opt(rough_pipeline_step),
            multispace0,
            lex_block_alt(vec![
                BlockKind::Nested(NestedBlockKind::Curly),
                BlockKind::Terminated(TerminatedBlockKind::Semicolon),
            ]),
        )),
    ))(input)
    .map(|(next, content)| {
        (
            next,
            LexBlock {
                kind: BlockKind::Partial,
                content,
                data: (),
            },
        )
    })
}

pub fn root_scope<I: Span>(input: I) -> Res<I, LexRootScope<I>> {
    context(
        "root-scope",
        tuple((
            root_scope_selector,
            multispace0,
            context("root-scope:block", cut(peek(tag("{")))),
            context(
                "root-scope:block",
                cut(lex_nested_block(NestedBlockKind::Curly)),
            ),
        )),
    )(input)
    .map(|(next, (selector, _, _, block))| {
        let scope = LexRootScope::new(selector, block);
        (next, scope)
    })
}

pub fn lex_scopes<I: Span>(input: I) -> Result<Vec<LexScope<I>>, SpaceErr> {
    if input.len() == 0 {
        return Ok(vec![]);
    }

    if wrapper(input.clone(), all_consuming(multispace1)).is_ok() {
        return Ok(vec![]);
    }

    result(
        context(
            "parsed-scopes",
            all_consuming(many0(delimited(
                multispace0,
                context(
                    "scope",
                    pair(peek(not(alt((tag("}"), eof)))), cut(lex_scope)),
                ),
                multispace0,
            ))),
        )(input)
        .map(|(next, scopes)| {
            let scopes: Vec<LexScope<I>> = scopes.into_iter().map(|scope| scope.1).collect();
            (next, scopes)
        }),
    )
}

/*
pub fn sub_scope_selector<I:Span>(input: Span) -> Res<Span, ScopeSelector<Span>> {
    alt((sub_scope_selector_expanded, sub_scope_selector_collapsed))
}




pub fn lex_scope_selector_no_filters(
    input: Span,
) -> Res<Span, ParsedScopeSelectorAndFilters<Span>> {
    context("parsed-scope-selector-no-filters", lex_scope_selector)(input)
        .map(|(next, selector)| (next, ParsedScopeSelectorAndFilters::new(selector, vec![])))
}

 */

pub fn next_stacked_name<I: Span>(input: I) -> Res<I, (I, Option<I>)> {
    match wrapper(
        input.clone(),
        pair(
            peek(tag("<")),
            tuple((
                tag("<"),
                pair(
                    context("scope-selector", alt((alphanumeric1, tag("*")))),
                    opt(recognize(nested_block(NestedBlockKind::Angle))),
                ),
                tag(">"),
            )),
        ),
    )
    .map(|(next, (_, (_, (name, children), _)))| (next, (name, children)))
    {
        Ok((next, (name, children))) => return Ok((next, (name, children))),
        Err(_) => {}
    }
    pair(
        context("scope-selector", cut(alt((alphanumeric1, tag("*"))))),
        opt(recognize(nested_block(NestedBlockKind::Angle))),
    )(input)
}

pub fn lex_scope_selector<I: Span>(input: I) -> Res<I, LexScopeSelector<I>> {
    let (next, ((name, children), filters, path)) = context(
        "parsed-scope-selector",
        tuple((next_stacked_name, scope_filters, opt(path_regex))),
    )(input.clone())?;

    Ok((next, LexScopeSelector::new(name, filters, path, children)))
}

pub fn lex_name_stack<I: Span>(mut input: I) -> Res<I, Vec<I>> {
    let mut stack = vec![];
    let (next, (name, mut children)) = next_stacked_name(input)?;
    stack.push(name);
    loop {
        match &children {
            None => {
                break;
            }
            Some(children) => {
                input = children.clone();
            }
        }
        let (_, (name, c)) = next_stacked_name(input)?;
        children = c;
        stack.push(name);
    }

    Ok((next, stack))
}

pub struct LexRouteSelector<I> {
    pub names: Vec<I>,
    pub filters: ScopeFiltersDef<I>,
    pub path: Option<I>,
}

pub fn lex_route_selector<I: Span>(input: I) -> Res<I, LexRouteSelector<I>> {
    tuple((lex_name_stack, scope_filters, opt(path_regex)))(input).map(
        |(next, (names, filters, path))| {
            let selector = LexRouteSelector {
                names,
                filters,
                path,
            };
            (next, selector)
        },
    )
}

pub fn wrapper<I: Span, O, F>(input: I, mut f: F) -> Res<I, O>
where
    F: FnMut(I) -> Res<I, O>,
{
    f.parse(input)
}

pub fn parse_inner_block<I, E, F>(
    kind: NestedBlockKind,
    mut f: &F,
) -> impl FnMut(I) -> IResult<I, I, E> + '_
where
    I: Span,
    &'static str: FindToken<<I as InputTakeAtPosition>::Item>,

    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Copy,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
    F: Fn(char) -> bool,
    F: Clone,
{
    move |input: I| {
        let (next, rtn) = alt((
            delimited(
                tag(kind.open()),
                recognize(many1(alt((
                    recognize(any_soround_lex_block),
                    recognize(verify(anychar, move |c| {
                        f(*c) && *c != kind.close_as_char()
                    })),
                )))),
                tag(kind.close()),
            ),
            recognize(many1(verify(anychar, move |c| {
                f(*c) && *c != kind.close_as_char()
            }))),
        ))(input)?;
        Ok((next, rtn))
    }
}

pub fn parse_include_blocks<I, O2, E, F>(
    kind: NestedBlockKind,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, I, E>
where
    I: Span,
    &'static str: FindToken<<I as InputTakeAtPosition>::Item>,

    I: ToString
        + InputLength
        + InputTake
        + Compare<&'static str>
        + InputIter
        + Clone
        + InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar,
    I: ToString,
    I: Offset + nom::Slice<std::ops::RangeTo<usize>>,
    I: nom::Slice<std::ops::RangeFrom<usize>>,
    <I as InputIter>::Item: AsChar,
    E: nom::error::ContextError<I> + nom::error::ParseError<I>,
    F: FnMut(I) -> IResult<I, O2, E>,
    F: Clone,
    <I as InputIter>::Item: std::marker::Copy,
{
    move |input: I| {
        recognize(many0(alt((
            recognize(any_soround_lex_block),
            recognize(verify(anychar, move |c| *c != kind.close_as_char())),
        ))))(input)
    }
}

pub fn scope_filters<I: Span>(input: I) -> Res<I, ScopeFiltersDef<I>> {
    pair(opt(scope_filter), many0(preceded(tag("-"), scope_filter)))(input).map(
        |(next, (first, mut many_filters))| {
            let mut filters = vec![];
            match first {
                None => {}
                Some(first) => {
                    filters.push(first);
                }
            }
            filters.append(&mut many_filters);
            let filters = ScopeFiltersDef { filters };
            (next, filters)
        },
    )
}

pub fn scope_filter<I: Span>(input: I) -> Res<I, ScopeFilterDef<I>> {
    delimited(
        tag("("),
        context(
            "scope-filter",
            cut(tuple((
                context("filter-name", cut(scope_name)),
                opt(context(
                    "filter-arguments",
                    preceded(
                        multispace1,
                        parse_include_blocks(NestedBlockKind::Parens, args),
                    ),
                )),
            ))),
        ),
        tag(")"),
    )(input)
    .map(|(next, (name, args))| {
        let filter = ScopeFilterDef { name, args };
        (next, filter)
    })
}

pub fn scope_name<I>(input: I) -> Res<I, I>
where
    I: Span,
{
    recognize(pair(
        skewer_case_chars,
        peek(alt((eof, multispace1, tag(")")))),
    ))(input)
}

pub fn root_scope_selector<I: Span>(input: I) -> Res<I, RootScopeSelector<I, Spanned<I, Version>>> {
    context(
        "root-scope-selector",
        cut(preceded(
            multispace0,
            pair(
                context("root-scope-selector:name", cut(root_scope_selector_name)),
                context("root-scope-selector:version", cut(scope_version)),
            ),
        )),
    )(input)
    .map(|(next, (name, version))| (next, RootScopeSelector { version, name }))
}

pub fn scope_version<I: Span>(input: I) -> Res<I, Spanned<I, Version>> {
    context(
        "scope-selector-version",
        tuple((
            tag("(version="),
            sub(version),
            context("scope-selector-version-closing-tag", tag(")")),
        )),
    )(input)
    .map(|((next, (_, version, _)))| (next, version))
}

/*
pub fn mytag<O>( tag: &str ) -> impl Fn(Span) -> Res<Span,O>
{
    move |i: Span| {
        let tag_len = tag.input_len();
        let t = tag.clone();
        let res: IResult<_, _, Error> = match i.compare(t) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            _ => {
                let e: ErrorKind = ErrorKind::Tag;
                Err(Err::Error(Error::from_error_kind(i, e)))
            }
        };
        res
    }
}

 */

pub fn scope_selector_name<I: Span>(input: I) -> Res<I, I> {
    context(
        "scope-selector-name",
        delimited(
            (context(
                "scope-selector-name:expect-alphanumeric-leading",
                cut(peek(alpha1)),
            )),
            alphanumeric1,
            context(
                "scope-selector-name:expect-termination",
                cut(peek(alt((
                    multispace1,
                    tag("{"),
                    tag("("),
                    tag("<"),
                    tag(">"),
                )))),
            ),
        ),
    )(input)
    .map(|(next, name)| (next, name))
}

pub fn root_scope_selector_name<I: Span>(input: I) -> Res<I, I> {
    context(
        "root-scope-selector-name",
        pair((peek(alpha1)), alphanumeric1),
    )(input)
    .map(|(next, (_, name))| (next, name))
}

pub fn lex_root_scope<I: Span>(span: I) -> Result<LexRootScope<I>, SpaceErr> {
    let root_scope = result(delimited(multispace0, root_scope, multispace0)(span))?;
    Ok(root_scope)
}

pub fn method_kind<I: Span>(input: I) -> Res<I, MethodKind> {
    let (next, v) = recognize(alt((tag("Cmd"), tag("Ext"), tag("Http"), tag("Hyp"))))(input)?;
    Ok((next, MethodKind::from_str(v.to_string().as_str()).unwrap()))
}

pub mod model {
    use std::collections::HashMap;
    use std::fmt::{Formatter, Write};
    use std::marker::PhantomData;
    use std::ops::{Deref, DerefMut};
    use std::rc::Rc;
    use std::str::FromStr;

    use bincode::Options;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alphanumeric1, multispace0, multispace1, satisfy};
    use nom::combinator::{cut, fail, not, peek, recognize, value};
    use nom::sequence::delimited;
    use regex::Regex;
    use serde::de::Visitor;
    use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

    use cosmic_nom::{new_span, Res, Span, Trace, Tw};

    use crate::command::direct::CmdKind;
    use crate::config::bind::{
        BindConfig, PipelineStepCtx, PipelineStepDef, PipelineStepVar, PipelineStopCtx,
        PipelineStopDef, PipelineStopVar, WaveDirection,
    };
    use crate::err::{ParseErrs, SpaceErr};
    use crate::loc::{Point, PointCtx, PointVar, Version};
    use crate::model::Env;
    use crate::parse::error::result;
    use crate::parse::{
        Assignment, camel_case_chars, CtxResolver, filepath_chars, http_method, lex_child_scopes,
        method_kind, pipeline, rc_command_type, ResolverErr,
        SubstParser, value_pattern, wrapped_cmd_method, wrapped_ext_method, wrapped_http_method,
        wrapped_sys_method,
    };
    use crate::util::{HttpMethodPattern, StringMatcher, ToResolved, ValueMatcher, ValuePattern};
    use crate::wave::core::http2::HttpMethod;
    use crate::wave::core::{DirectedCore, Method, MethodKind};
    use crate::wave::{DirectedWave, Ping, SingularDirectedWave};

    #[derive(Clone)]
    pub struct ScopeSelectorAndFiltersDef<S, I> {
        pub selector: S,
        pub filters: ScopeFiltersDef<I>,
    }

    impl<S, I> Deref for ScopeSelectorAndFiltersDef<S, I> {
        type Target = S;

        fn deref(&self) -> &Self::Target {
            &self.selector
        }
    }

    impl<S, I> ScopeSelectorAndFiltersDef<S, I> {
        pub fn new(selector: S, filters: ScopeFiltersDef<I>) -> Self {
            Self { selector, filters }
        }
    }

    pub enum ParsePhase {
        Root,
        SubScopes,
    }

    #[derive(Clone)]
    pub struct Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        pub span: I,
        pub element: E,
    }

    impl<I, E> Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        pub fn new(element: E, span: I) -> Spanned<I, E> {
            Self { span, element }
        }
    }

    impl<I, E> Spanned<I, E>
    where
        E: Clone + ToString,
        I: ToString,
    {
        pub fn len(&self) -> usize {
            self.element.to_string().len()
        }
    }

    impl<I, E> ToString for Spanned<I, E>
    where
        E: Clone + ToString,
        I: ToString,
    {
        fn to_string(&self) -> String {
            self.element.to_string()
        }
    }

    impl<I, E> Deref for Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        type Target = E;

        fn deref(&self) -> &Self::Target {
            &self.element
        }
    }

    impl<I, E> DerefMut for Spanned<I, E>
    where
        E: Clone,
        I: ToString,
    {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.element
        }
    }

    #[derive(Clone, Eq, PartialEq, Hash)]
    pub struct RootScopeSelector<I, V> {
        pub name: I,
        pub version: V,
    }

    impl<I, V> RootScopeSelector<I, V> {
        pub fn new(name: I, version: V) -> Self {
            RootScopeSelector { name, version }
        }
    }

    impl<I: ToString, V: ToString> RootScopeSelector<I, V> {
        pub fn to_concrete(self) -> Result<RootScopeSelector<String, Version>, SpaceErr> {
            Ok(RootScopeSelector {
                name: self.name.to_string(),
                version: Version::from_str(self.version.to_string().as_str())?,
            })
        }
    }

    impl RouteScope {
        pub fn select(&self, directed: &DirectedWave) -> Vec<&WaveScope> {
            let mut scopes = vec![];
            for scope in &self.block {
                if scope.selector.is_match(directed).is_ok() {
                    scopes.push(scope);
                }
            }
            scopes
        }
    }

    #[derive(Clone)]
    pub struct RouteScopeSelector {
        pub selector: ScopeSelectorDef<String, Regex>,
    }

    impl RouteScopeSelector {
        pub fn new<I: ToString>(path: Option<I>) -> Result<Self, SpaceErr> {
            let path = match path {
                None => Regex::new(".*")?,
                Some(path) => Regex::new(path.to_string().as_str())?,
            };
            Ok(Self {
                selector: ScopeSelectorDef {
                    path,
                    name: "Route".to_string(),
                },
            })
        }

        pub fn from<I: ToString>(selector: LexScopeSelector<I>) -> Result<Self, SpaceErr> {
            if selector.name.to_string().as_str() != "Route" {
                return Err(SpaceErr::server_error("expected Route"));
            }
            let path = match selector.path {
                None => None,
                Some(path) => Some(path.to_string()),
            };

            Ok(RouteScopeSelector::new(path)?)
        }
    }

    impl Deref for RouteScopeSelector {
        type Target = ScopeSelectorDef<String, Regex>;

        fn deref(&self) -> &Self::Target {
            &self.selector
        }
    }

    #[derive(Clone)]
    pub struct ScopeSelectorDef<N, P> {
        pub name: N,
        pub path: P,
    }

    impl ValueMatcher<DirectedWave> for RouteScopeSelector {
        fn is_match(&self, directed: &DirectedWave) -> Result<(), ()> {
            if self.name.as_str() != "Route" {
                return Err(());
            }
            match self.selector.path.is_match(&directed.core().uri.path()) {
                true => Ok(()),
                false => Err(()),
            }
        }
    }

    impl ValueMatcher<SingularDirectedWave> for RouteScopeSelector {
        fn is_match(&self, directed: &SingularDirectedWave) -> Result<(), ()> {
            if self.name.as_str() != "Route" {
                return Err(());
            }
            match self.selector.path.is_match(&directed.core().uri.path()) {
                true => Ok(()),
                false => Err(()),
            }
        }
    }

    impl ValueMatcher<DirectedWave> for MessageScopeSelector {
        fn is_match(&self, directed: &DirectedWave) -> Result<(), ()> {
            self.name.is_match(&directed.core().method.kind())?;
            match self.path.is_match(&directed.core().uri.path()) {
                true => Ok(()),
                false => Err(()),
            }
        }
    }

    impl ValueMatcher<SingularDirectedWave> for MessageScopeSelector {
        fn is_match(&self, directed: &SingularDirectedWave) -> Result<(), ()> {
            self.name.is_match(&directed.core().method.kind())?;
            match self.path.is_match(&directed.core().uri.path()) {
                true => Ok(()),
                false => Err(()),
            }
        }
    }

    fn default_path<I: ToString>(path: Option<I>) -> Result<Regex, SpaceErr> {
        match path {
            None => Ok(Regex::new(".*")?),
            Some(path) => Ok(Regex::new(path.to_string().as_str())?),
        }
    }
    impl WaveScope {
        pub fn from_scope<I: Span>(scope: LexParentScope<I>) -> Result<Self, SpaceErr> {
            let selector = MessageScopeSelectorAndFilters::from_selector(scope.selector)?;
            let mut block = vec![];

            for scope in scope.block.into_iter() {
                let method = MethodScope::from_scope(&selector.selector.name, scope)?;
                block.push(method);
            }

            Ok(Self { selector, block })
        }

        pub fn select(&self, directed: &DirectedWave) -> Vec<&MethodScope> {
            let mut scopes = vec![];
            for scope in &self.block {
                if scope.selector.is_match(directed).is_ok() {
                    scopes.push(scope);
                }
            }
            scopes
        }
    }

    impl MessageScopeSelectorAndFilters {
        pub fn from_selector<I: Span>(selector: LexScopeSelector<I>) -> Result<Self, SpaceErr> {
            let filters = selector.filters.clone().to_scope_filters();
            let selector = MessageScopeSelector::from_selector(selector)?;
            Ok(Self { selector, filters })
        }
    }

    impl RouteScopeSelectorAndFilters {
        pub fn from_selector<I: Span>(selector: LexScopeSelector<I>) -> Result<Self, SpaceErr> {
            let filters = selector.filters.clone().to_scope_filters();
            let selector = RouteScopeSelector::new(selector.path.clone())?;
            Ok(Self { selector, filters })
        }
    }

    impl ValueMatcher<DirectedWave> for RouteScopeSelectorAndFilters {
        fn is_match(&self, request: &DirectedWave) -> Result<(), ()> {
            // nothing for filters at this time...
            self.selector.is_match(request)
        }
    }

    impl ValueMatcher<SingularDirectedWave> for RouteScopeSelectorAndFilters {
        fn is_match(&self, wave: &SingularDirectedWave) -> Result<(), ()> {
            // nothing for filters at this time...
            self.selector.is_match(wave)
        }
    }

    impl ValueMatcher<DirectedWave> for MessageScopeSelectorAndFilters {
        fn is_match(&self, request: &DirectedWave) -> Result<(), ()> {
            // nothing for filters at this time...
            self.selector.is_match(request)
        }
    }

    impl ValueMatcher<SingularDirectedWave> for MessageScopeSelectorAndFilters {
        fn is_match(&self, request: &SingularDirectedWave) -> Result<(), ()> {
            // nothing for filters at this time...
            self.selector.is_match(request)
        }
    }

    impl ValueMatcher<DirectedWave> for MethodScopeSelectorAndFilters {
        fn is_match(&self, directed: &DirectedWave) -> Result<(), ()> {
            // nothing for filters at this time...
            self.selector.is_match(directed)
        }
    }

    impl ValueMatcher<SingularDirectedWave> for MethodScopeSelectorAndFilters {
        fn is_match(&self, directed: &SingularDirectedWave) -> Result<(), ()> {
            // nothing for filters at this time...
            self.selector.is_match(directed)
        }
    }

    impl MethodScope {
        pub fn from_scope<I: Span>(
            parent: &ValuePattern<MethodKind>,
            scope: LexScope<I>,
        ) -> Result<Self, SpaceErr> {
            let selector = MethodScopeSelectorAndFilters::from_selector(parent, scope.selector)?;
            let block = result(pipeline(scope.block.content))?;
            Ok(Self { selector, block })
        }
    }

    impl MessageScopeSelector {
        pub fn from_selector<I: Span>(selector: LexScopeSelector<I>) -> Result<Self, SpaceErr> {
            let kind = match result(value_pattern(method_kind)(selector.name.clone())) {
                Ok(kind) => kind,
                Err(_) => {
                    return Err(ParseErrs::from_loc_span(
                        format!(
                            "unknown MessageKind: {} valid message kinds: Ext, Http, Cmd or *",
                            selector.name.to_string()
                        )
                        .as_str(),
                        "unknown message kind",
                        selector.name,
                    ));
                }
            };

            Ok(Self {
                name: kind,
                path: default_path(selector.path)?,
            })
        }
    }

    impl ValueMatcher<DirectedWave> for MethodScopeSelector {
        fn is_match(&self, directed: &DirectedWave) -> Result<(), ()> {
            self.name.is_match(&directed.core().method)?;
            match self.path.is_match(&directed.core().uri.path()) {
                true => Ok(()),
                false => Err(()),
            }
        }
    }

    impl ValueMatcher<SingularDirectedWave> for MethodScopeSelector {
        fn is_match(&self, directed: &SingularDirectedWave) -> Result<(), ()> {
            self.name.is_match(&directed.core().method)?;
            match self.path.is_match(&directed.core().uri.path()) {
                true => Ok(()),
                false => Err(()),
            }
        }
    }
    impl MethodScopeSelectorAndFilters {
        pub fn from_selector<I: Span>(
            parent: &ValuePattern<MethodKind>,
            selector: LexScopeSelector<I>,
        ) -> Result<Self, SpaceErr> {
            let filters = selector.filters.clone().to_scope_filters();
            let selector = MethodScopeSelector::from_selector(parent, selector)?;
            Ok(Self { selector, filters })
        }
    }

    impl MethodScopeSelector {
        pub fn from_selector<I: Span>(
            parent: &ValuePattern<MethodKind>,
            selector: LexScopeSelector<I>,
        ) -> Result<Self, SpaceErr> {
            let name = match parent {
                ValuePattern::Any => ValuePattern::Any,
                ValuePattern::None => ValuePattern::None,
                ValuePattern::Pattern(message_kind) => match message_kind {
                    MethodKind::Hyp => {
                        match result(value_pattern(wrapped_sys_method)(selector.name.clone())) {
                            Ok(r) => r,
                            Err(_) => {
                                return Err(ParseErrs::from_loc_span(
                                    format!(
                                        "invalid Hyp method '{}'.  Hyp should be CamelCase",
                                        selector.name.to_string()
                                    )
                                    .as_str(),
                                    "invalid Hyp",
                                    selector.name,
                                ))
                            }
                        }
                    }
                    MethodKind::Cmd => {
                        match result(value_pattern(wrapped_cmd_method)(selector.name.clone())) {
                            Ok(r) => r,
                            Err(_) => {
                                return Err(ParseErrs::from_loc_span(
                                    format!(
                                        "invalid Cmd method '{}'.  Cmd should be CamelCase",
                                        selector.name.to_string()
                                    )
                                    .as_str(),
                                    "invalid Cmd",
                                    selector.name,
                                ))
                            }
                        }
                    }
                    MethodKind::Ext => {
                        match result(value_pattern(wrapped_ext_method)(selector.name.clone())) {
                            Ok(r) => r,
                            Err(_) => {
                                return Err(ParseErrs::from_loc_span(
                                    format!(
                                        "invalid Ext method '{}'.  Ext should be CamelCase",
                                        selector.name.to_string()
                                    )
                                    .as_str(),
                                    "invalid Ext",
                                    selector.name,
                                ))
                            }
                        }
                    }
                    MethodKind::Http => {
                        match result(value_pattern( wrapped_http_method)(selector.name.clone())) {
                                Ok(r) => r,
                                Err(_) => {
                                    return Err(ParseErrs::from_loc_span(format!("invalid Http Pattern '{}'.  Http should be camel case 'Get' and a valid Http method", selector.name.to_string()).as_str(), "invalid Http method", selector.name ))
                                }
                            }
                    }
                },
            };

            Ok(Self {
                name,
                path: default_path(selector.path)?,
            })
        }
    }

    impl<N, P> ScopeSelectorDef<N, P> {
        pub fn new(name: N, path: P) -> Self {
            Self { name, path }
        }
    }

    #[derive(Clone)]
    pub struct LexScopeSelector<I> {
        pub name: I,
        pub filters: ScopeFiltersDef<I>,
        pub children: Option<I>,
        pub path: Option<I>,
    }

    impl<I: ToString> LexScopeSelector<I> {
        pub fn new(
            name: I,
            filters: ScopeFiltersDef<I>,
            path: Option<I>,
            children: Option<I>,
        ) -> Self {
            Self {
                name,
                filters,
                children,
                path,
            }
        }
    }

    impl<I> LexScopeSelector<I> {
        pub fn has_children(&self) -> bool {
            self.children.is_some()
        }
    }

    #[derive(Clone)]
    pub struct ScopeFiltersDef<I> {
        pub filters: Vec<ScopeFilterDef<I>>,
    }

    impl Default for ScopeFilters {
        fn default() -> Self {
            Self { filters: vec![] }
        }
    }

    impl<I> Deref for ScopeFiltersDef<I> {
        type Target = Vec<ScopeFilterDef<I>>;

        fn deref(&self) -> &Self::Target {
            &self.filters
        }
    }

    impl<I> ScopeFiltersDef<I> {
        pub fn is_empty(&self) -> bool {
            self.filters.is_empty()
        }
    }

    impl<I: ToString> ScopeFiltersDef<I> {
        pub fn to_scope_filters(self) -> ScopeFilters {
            ScopeFilters {
                filters: self
                    .filters
                    .into_iter()
                    .map(|f| f.to_scope_filter())
                    .collect(),
            }
        }

        pub fn len(&self) -> usize {
            self.filters.len()
        }

        pub fn empty() -> Self {
            Self { filters: vec![] }
        }
    }

    #[derive(Clone)]
    pub struct ScopeFilterDef<I> {
        pub name: I,
        pub args: Option<I>,
    }

    impl<I: ToString> ScopeFilterDef<I> {
        pub fn to_scope_filter(self) -> ScopeFilter {
            ScopeFilter {
                name: self.name.to_string(),
                args: match self.args {
                    None => None,
                    Some(args) => Some(args.to_string()),
                },
            }
        }
    }

    pub type RegexStr = String;
    pub type ScopeFilter = ScopeFilterDef<String>;
    pub type ScopeFilters = ScopeFiltersDef<String>;
    pub type LexBlock<I> = Block<I, ()>;
    pub type LexRootScope<I> = Scope<RootScopeSelector<I, Spanned<I, Version>>, Block<I, ()>, I>;
    pub type LexScope<I> = Scope<LexScopeSelector<I>, Block<I, ()>, I>;
    pub type LexParentScope<I> = Scope<LexScopeSelector<I>, Vec<LexScope<I>>, I>;

    //pub type LexPipelineScope<I> = PipelineScopeDef<I, VarPipeline>;
    pub type PipelineSegmentCtx = PipelineSegmentDef<PointCtx>;
    pub type PipelineSegmentVar = PipelineSegmentDef<PointVar>;

    #[derive(Debug, Clone)]
    pub struct PipelineSegmentDef<Pnt> {
        pub step: PipelineStepDef<Pnt>,
        pub stop: PipelineStopDef<Pnt>,
    }

    impl ToResolved<PipelineSegment> for PipelineSegmentVar {
        fn to_resolved(self, env: &Env) -> Result<PipelineSegment, SpaceErr> {
            let rtn: PipelineSegmentCtx = self.to_resolved(env)?;
            rtn.to_resolved(env)
        }
    }

    impl ToResolved<PipelineSegment> for PipelineSegmentCtx {
        fn to_resolved(self, env: &Env) -> Result<PipelineSegment, SpaceErr> {
            Ok(PipelineSegment {
                step: self.step.to_resolved(env)?,
                stop: self.stop.to_resolved(env)?,
            })
        }
    }

    impl ToResolved<PipelineSegmentCtx> for PipelineSegmentVar {
        fn to_resolved(self, env: &Env) -> Result<PipelineSegmentCtx, SpaceErr> {
            Ok(PipelineSegmentCtx {
                step: self.step.to_resolved(env)?,
                stop: self.stop.to_resolved(env)?,
            })
        }
    }

    /*
    impl CtxSubst<PipelineSegment> for PipelineSegmentCtx{
        fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<PipelineSegment, ExtErr> {
            let mut errs = vec![];
            let step = match self.step.resolve_ctx(resolver) {
                Ok(step) => Some(step),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            let stop = match self.stop.resolve_ctx(resolver) {
                Ok(stop) => Some(stop),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };
            if errs.is_empty() {
                Ok(PipelineSegment {
                    step: step.expect("step"),
                    stop: stop.expect("stop")
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

     */

    pub type PipelineSegment = PipelineSegmentDef<Point>;
    pub type RouteScope = ScopeDef<RouteScopeSelectorAndFilters, Vec<WaveScope>>;
    pub type WaveScope = ScopeDef<MessageScopeSelectorAndFilters, Vec<MethodScope>>;
    pub type MethodScope = ScopeDef<MethodScopeSelectorAndFilters, PipelineVar>;
    //    pub type ValuePatternScopeSelector = ScopeSelectorDef<ValuePattern<String>, String,Regex>;
    pub type MessageScopeSelector = ScopeSelectorDef<ValuePattern<MethodKind>, Regex>;
    pub type MethodScopeSelector = ScopeSelectorDef<ValuePattern<Method>, Regex>;
    pub type RouteScopeSelectorAndFilters = ScopeSelectorAndFiltersDef<RouteScopeSelector, String>;
    pub type MessageScopeSelectorAndFilters =
        ScopeSelectorAndFiltersDef<MessageScopeSelector, String>;
    pub type MethodScopeSelectorAndFilters =
        ScopeSelectorAndFiltersDef<MethodScopeSelector, String>;

    /*    pub type ValuePatternScopeSelectorAndFilters =
           ScopeSelectorAndFiltersDef<ValuePatternScopeSelector, String>;

    */
    pub type LexScopeSelectorAndFilters<I> = ScopeSelectorAndFiltersDef<LexScopeSelector<I>, I>;
    //    pub type Pipeline = Vec<PipelineSegment>;

    impl<I: Span> TryFrom<LexParentScope<I>> for RouteScope {
        type Error = SpaceErr;

        fn try_from(scope: LexParentScope<I>) -> Result<Self, Self::Error> {
            let mut errs = vec![];
            let mut message_scopes = vec![];
            let route_selector = RouteScopeSelectorAndFilters::from_selector(scope.selector)?;
            for message_scope in scope.block {
                match lex_child_scopes(message_scope) {
                    Ok(message_scope) => match WaveScope::from_scope(message_scope) {
                        Ok(message_scope) => message_scopes.push(message_scope),
                        Err(err) => errs.push(err),
                    },
                    Err(err) => {
                        errs.push(err);
                    }
                }
            }
            if errs.is_empty() {
                Ok(RouteScope {
                    selector: route_selector,
                    block: message_scopes,
                })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

    /*
    impl<I: Span> LexScopeSelectorAndFilters<I> {
        pub fn to_value_pattern_scope_selector(
            self,
        ) -> Result<ValuePatternScopeSelectorAndFilters, ExtErr> {
            Ok(ValuePatternScopeSelectorAndFilters {
                selector: self.selector.to_value_pattern_scope_selector()?,
                filters: self.filters.to_scope_filters(),
            })
        }
    }

     */

    /*
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct VarPipelineSegmentDef<Step, Stop> {
        pub step: Step,
        pub stop: Stop,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PipelineSegmentDef<Pnt> {
        pub step: PipelineStepDef<Pnt>,
        pub stop: PipelineStopDef<Pnt>,
    }

     */

    pub type Pipeline = PipelineDef<PipelineSegment>;
    pub type PipelineCtx = PipelineDef<PipelineSegmentCtx>;
    pub type PipelineVar = PipelineDef<PipelineSegmentVar>;

    impl ToResolved<Pipeline> for PipelineCtx {
        fn to_resolved(self, env: &Env) -> Result<Pipeline, SpaceErr> {
            let mut segments = vec![];
            for segment in self.segments.into_iter() {
                segments.push(segment.to_resolved(env)?);
            }

            Ok(Pipeline { segments })
        }
    }

    impl ToResolved<PipelineCtx> for PipelineVar {
        fn to_resolved(self, env: &Env) -> Result<PipelineCtx, SpaceErr> {
            let mut segments = vec![];
            for segment in self.segments.into_iter() {
                segments.push(segment.to_resolved(env)?);
            }

            Ok(PipelineCtx { segments })
        }
    }

    /*
    impl CtxSubst<Pipeline> for PipelineCtx {
        fn resolve_ctx(self, resolver: &dyn CtxResolver) -> Result<Pipeline, ExtErr> {
            let mut errs = vec![];
            let mut segments = vec![];
            for segment in self.segments {
                match segment.resolve_ctx(resolver) {
                    Ok(segment) => segments.push(segment),
                    Err(err) => errs.push(err)
                }
            }
            if errs.is_empty() {
                Ok( Pipeline { segments })
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

     */

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PipelineDef<S> {
        pub segments: Vec<S>,
    }

    impl<S> PipelineDef<S> {
        pub fn new() -> Self {
            Self { segments: vec![] }
        }
    }

    impl<S> Deref for PipelineDef<S> {
        type Target = Vec<S>;

        fn deref(&self) -> &Self::Target {
            &self.segments
        }
    }

    impl<S> DerefMut for PipelineDef<S> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.segments
        }
    }

    impl<S> PipelineDef<S> {
        pub fn consume(&mut self) -> Option<S> {
            if self.segments.is_empty() {
                None
            } else {
                Some(self.segments.remove(0))
            }
        }
    }

    /*
    impl <I:Span> VarSubst<PipelineCtx> for VarPipeline<I> {
        fn resolve_vars(self, resolver: &dyn VarResolver) -> Result<PipelineCtx, ExtErr> {
            let mut pipeline = PipelineCtx::new();
            let mut errs = vec![];
            for segment in self.segments {
                match segment.resolve_vars(resolver) {
                    Ok(segment) => {
                        pipeline.segments.push(segment);
                    }
                    Err(err) => {
                        errs.push(err);
                    }
                }
            }

            if errs.is_empty() {
                Ok(pipeline)
            } else {
                Err(ParseErrs::fold(errs).into())
            }
        }
    }

     */

    /*
    impl <I:Span> VarSubst<PipelineSegmentCtx> for VarPipelineSegment<I> {
        fn resolve_vars(self, resolver: &dyn VarResolver) -> Result<PipelineSegmentCtx, ExtErr> {
            unimplemented!()
            /*
            let mut errs = vec![];

            if self.stop.is_none() {
                errs.push(ParseErrs::from_owned_span(
                    "expecting Pipeline Stop to follow Pipeline Step",
                    "Needs a following Pipeline Stop",
                    self.step.span(),
                ));
            }

            let step = match self.step.resolve(resolver) {
                Ok(step) => Some(step),
                Err(err) => {
                    errs.push(err);
                    None
                }
            };

            let stop = match self.stop {
                Some(stop) => match stop.resolve(resolver) {
                    Ok(stop) => Some(stop),
                    Err(err) => {
                        errs.push(err);
                        None
                    }
                },
                None => None,
            };

            if step.is_some() && stop.is_some() && errs.is_empty() {
                let step = step.expect("step");
                let stop = stop.expect("stop");
                Ok(PipelineSegmentCtx { step, stop })
            } else {
                Err(ParseErrs::fold(errs).into())
            }

             */
        }
    }

     */

    #[derive(Clone)]
    pub enum MechtronScope {
        WasmScope(Vec<Assignment>),
    }

    #[derive(Clone)]
    pub enum BindScope {
        RequestScope(RouteScope),
    }

    #[derive(Debug, Clone)]
    pub struct ScopeDef<S, B> {
        pub selector: S,
        pub block: B,
    }

    #[derive(Clone)]
    pub enum BindScopeKind {
        Pipelines,
    }

    #[derive(Clone)]
    pub enum BuiltInFilter {
        Auth,
        NoAuth,
    }

    #[derive(Clone)]
    pub struct Scope<S, B, P>
    where
        S: Clone,
    {
        pub selector: S,
        pub pipeline_step: Option<P>,
        pub block: B,
    }

    impl<S, B, P> Scope<S, B, P>
    where
        S: Clone,
    {
        pub fn new(selector: S, block: B) -> Self {
            Self {
                selector,
                block,
                pipeline_step: None,
            }
        }

        pub fn new_with_pipeline_step(selector: S, block: B, pipeline_step: Option<P>) -> Self {
            Self {
                selector,
                block,
                pipeline_step,
            }
        }
    }

    impl<S, FromBlock, P> Scope<S, FromBlock, P>
    where
        S: Clone,
    {
        pub fn upgrade<ToBlock>(self, block: ToBlock) -> Scope<S, ToBlock, P> {
            Scope {
                selector: self.selector,
                block,
                pipeline_step: self.pipeline_step,
            }
        }
    }

    #[derive(Clone)]
    pub struct Block<I, D> {
        pub kind: BlockKind,
        pub content: I,
        pub data: D,
    }

    impl<I> Block<I, ()> {
        pub fn parse(kind: BlockKind, content: I) -> Block<I, ()> {
            Block {
                kind,
                content,
                data: (),
            }
        }
    }

    #[derive(Debug, Copy, Clone, strum_macros::Display, Eq, PartialEq)]
    pub enum BlockKind {
        Nested(NestedBlockKind),
        Terminated(TerminatedBlockKind),
        Delimited(DelimitedBlockKind),
        Partial,
    }

    #[derive(Debug, Copy, Clone, strum_macros::Display, Eq, PartialEq)]
    pub enum TerminatedBlockKind {
        Semicolon,
    }

    impl TerminatedBlockKind {
        pub fn tag(&self) -> &'static str {
            match self {
                TerminatedBlockKind::Semicolon => ";",
            }
        }

        pub fn as_char(&self) -> char {
            match self {
                TerminatedBlockKind::Semicolon => ';',
            }
        }
    }

    #[derive(
        Debug, Copy, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq,
    )]
    pub enum DelimitedBlockKind {
        SingleQuotes,
        DoubleQuotes,
    }

    impl DelimitedBlockKind {
        pub fn delim(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "'",
                DelimitedBlockKind::DoubleQuotes => "\"",
            }
        }

        pub fn escaped(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "\'",
                DelimitedBlockKind::DoubleQuotes => "\"",
            }
        }

        pub fn context(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "single:quotes:block",
                DelimitedBlockKind::DoubleQuotes => "double:quotes:block",
            }
        }

        pub fn missing_close_context(&self) -> &'static str {
            match self {
                DelimitedBlockKind::SingleQuotes => "single:quotes:block:missing-close",
                DelimitedBlockKind::DoubleQuotes => "double:quotes:block:missing-close",
            }
        }
    }

    #[derive(
        Debug, Copy, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq,
    )]
    pub enum NestedBlockKind {
        Curly,
        Parens,
        Square,
        Angle,
    }

    impl NestedBlockKind {
        pub fn is_block_terminator(c: char) -> bool {
            match c {
                '}' => true,
                ')' => true,
                ']' => true,
                '>' => true,
                _ => false,
            }
        }

        pub fn error_message<I: Span>(span: &I, context: &str) -> Result<&'static str, ()> {
            if Self::Curly.open_context() == context {
                Ok("expecting '{' (open scope block)")
            } else if Self::Parens.open_context() == context {
                Ok("expecting '(' (open scope block)")
            } else if Self::Angle.open_context() == context {
                Ok("expecting '<' (open scope block)")
            } else if Self::Square.open_context() == context {
                Ok("expecting '[' (open scope block)")
            } else if Self::Curly.close_context() == context {
                Ok("expecting '}' (close scope block)")
            } else if Self::Parens.close_context() == context {
                Ok("expecting ')' (close scope block)")
            } else if Self::Angle.close_context() == context {
                Ok("expecting '>' (close scope block)")
            } else if Self::Square.close_context() == context {
                Ok("expecting ']' (close scope block)")
            } else if Self::Curly.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else if Self::Parens.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else if Self::Angle.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else if Self::Square.unpaired_closing_scope() == context {
                Ok("closing scope without an opening scope")
            } else {
                Err(())
            }
        }

        pub fn context(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:{}",
                NestedBlockKind::Parens => "block:()",
                NestedBlockKind::Square => "block:[]",
                NestedBlockKind::Angle => "block:<>",
            }
        }

        pub fn open_context(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:open:{",
                NestedBlockKind::Parens => "block:open:(",
                NestedBlockKind::Square => "block:open:[",
                NestedBlockKind::Angle => "block:open:<",
            }
        }

        pub fn close_context(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:close:}",
                NestedBlockKind::Parens => "block:close:)",
                NestedBlockKind::Square => "block:close:]",
                NestedBlockKind::Angle => "block:close:>",
            }
        }

        pub fn unpaired_closing_scope(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "block:close-before-open:}",
                NestedBlockKind::Parens => "block:close-before-open:)",
                NestedBlockKind::Square => "block:close-before-open:]",
                NestedBlockKind::Angle => "block:close-before-open:>",
            }
        }

        pub fn open(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "{",
                NestedBlockKind::Parens => "(",
                NestedBlockKind::Square => "[",
                NestedBlockKind::Angle => "<",
            }
        }

        pub fn close(&self) -> &'static str {
            match self {
                NestedBlockKind::Curly => "}",
                NestedBlockKind::Parens => ")",
                NestedBlockKind::Square => "]",
                NestedBlockKind::Angle => ">",
            }
        }

        pub fn open_as_char(&self) -> char {
            match self {
                NestedBlockKind::Curly => '{',
                NestedBlockKind::Parens => '(',
                NestedBlockKind::Square => '[',
                NestedBlockKind::Angle => '<',
            }
        }

        pub fn close_as_char(&self) -> char {
            match self {
                NestedBlockKind::Curly => '}',
                NestedBlockKind::Parens => ')',
                NestedBlockKind::Square => ']',
                NestedBlockKind::Angle => '>',
            }
        }
    }

    pub enum TextType<I> {
        Comment(I),
        NoComment(I),
    }

    impl<I: ToString> ToString for TextType<I> {
        fn to_string(&self) -> String {
            match self {
                TextType::Comment(i) => i.to_string(),
                TextType::NoComment(i) => i.to_string(),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum Chunk<I> {
        Var(I),
        Text(I),
    }
    impl<I> Chunk<I>
    where
        I: Span,
    {
        pub fn stringify(self) -> Chunk<Tw<String>> {
            match self {
                Chunk::Var(var) => Chunk::Var(Tw::new(var.clone(), var.to_string())),
                Chunk::Text(text) => Chunk::Text(Tw::new(text.clone(), text.to_string())),
            }
        }
    }

    impl<I> Chunk<I> {
        pub fn span(&self) -> &I {
            match self {
                Chunk::Var(var) => var,
                Chunk::Text(text) => text,
            }
        }
    }

    impl<I: ToString> Chunk<I> {
        pub fn len(&self) -> usize {
            match self {
                Chunk::Var(var) => {
                    // account for ${}
                    var.to_string().len() + 3
                }
                Chunk::Text(text) => text.to_string().len(),
            }
        }
    }

    #[derive(Clone)]
    pub enum Var<O, P>
    where
        P: VarParser<O>,
    {
        Val(O),
        Var { name: String, parser: P },
    }

    pub trait VarParser<O> {
        fn parse<I: Span>(input: I) -> Result<O, SpaceErr>;
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct Subst<I> {
        pub chunks: Vec<Chunk<I>>,
        pub trace: Trace,
    }

    impl Subst<Tw<String>> {
        pub fn new(path: &str) -> Result<Self, SpaceErr> {
            let path = result(crate::parse::subst_path(new_span(path)))?;
            Ok(path.stringify())
        }
    }

    impl<I> Subst<I>
    where
        I: Span,
    {
        pub fn stringify(self) -> Subst<Tw<String>> {
            let chunks: Vec<Chunk<Tw<String>>> =
                self.chunks.into_iter().map(|c| c.stringify()).collect();
            Subst {
                chunks,
                trace: self.trace,
            }
        }
    }

    impl<I> ToString for Subst<I>
    where
        I: ToString,
    {
        fn to_string(&self) -> String {
            let mut rtn = String::new();
            for chunk in &self.chunks {
                match chunk {
                    Chunk::Var(var) => {
                        rtn.push_str(format!("${{{}}}", var.to_string()).as_str());
                    }
                    Chunk::Text(text) => {
                        rtn.push_str(text.to_string().as_str());
                    }
                }
            }
            rtn
        }
    }

    impl ToResolved<String> for Subst<Tw<String>> {
        fn to_resolved(self, env: &Env) -> Result<String, SpaceErr> {
            let mut rtn = String::new();
            let mut errs = vec![];
            for chunk in self.chunks {
                match chunk {
                    Chunk::Var(var) => match env.val(var.to_string().as_str()) {
                        Ok(val) => {
                            let val: String = val.clone().try_into()?;
                            rtn.push_str(val.as_str());
                        }
                        Err(err) => {
                            errs.push(ParseErrs::from_range(
                                format!("could not find variable: {}", var.to_string()).as_str(),
                                "not found",
                                var.trace.range,
                                var.trace.extra,
                            ));
                        }
                    },
                    Chunk::Text(text) => {
                        rtn.push_str(text.to_string().as_str());
                    }
                }
            }

            if errs.is_empty() {
                Ok(rtn)
            } else {
                let errs = ParseErrs::fold(errs);
                Err(errs.into())
            }
        }
    }
}

pub mod error {
    use core::str::FromStr;
    use std::collections::HashMap;
    use std::sync::Arc;
    //    use ariadne::Report;
    //    use ariadne::{Label, ReportKind, Source};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{
        alpha1, alphanumeric0, alphanumeric1, digit1, multispace0, multispace1, satisfy, space1,
    };
    use nom::combinator::{all_consuming, cut, fail, not, opt, peek, recognize, value};
    use nom::error::{context, ContextError, ErrorKind, ParseError};
    use nom::multi::{many0, many1, separated_list0};
    use nom::sequence::{delimited, pair, preceded, terminated, tuple};
    use nom::{
        AsChar, Compare, Err, InputLength, InputTake, InputTakeAtPosition, IResult, Parser, Slice,
    };
    use nom_supreme::error::{BaseErrorKind, ErrorTree, StackContext};
    use regex::internal::Input;
    use regex::{Error, Regex};

    use cosmic_nom::{len, new_span, Res, Span, span_with_extra, trim, tw};

    use crate::command::direct::CmdKind;
    use crate::command::CommandVar;
    use crate::config::bind::{PipelineStepVar, PipelineStopVar, RouteSelector, WaveDirection};
    use crate::err::report::{Label, Report, ReportKind};
    use crate::err::{ParseErrs, SpaceErr};
    use crate::loc::{Layer, PointSeg, PointVar, StarKey, Topic, VarVal, Version};
    use crate::model::CamelCase;
    use crate::parse::model::{
        BindScope, BindScopeKind, BlockKind, Chunk, DelimitedBlockKind, LexScope, NestedBlockKind,
        PipelineSegmentVar, PipelineVar, RouteScope, Spanned, Subst, TextType,
    };
    use crate::parse::{
        any_block, any_soround_lex_block, camel_case, camel_case_chars,
        camel_case_to_string_matcher, domain, file_chars, filepath_chars, get, lex_child_scopes,
        lex_root_scope, lex_route_selector, lex_scopes, lowercase_alphanumeric, method_kind,
        nospace1, parse_uuid, point_segment_chars, point_var, rec_version, select, set, skewer,
        skewer_case, skewer_chars, subst_path, SubstParser, unwrap_block, variable_name,
        version_chars, version_req_chars,
    };
    use crate::particle::PointKindVar;
    use crate::selector::{
        UploadBlock, VersionReq,
    };
    use crate::substance::{
        CallKind, CallVar, CallWithConfigVar, ExtCall, HttpCall, ListPattern, MapPatternVar,
        NumRange, SubstanceFormat, SubstanceKind, SubstancePattern, SubstancePatternVar,
        SubstanceTypePatternDef, SubstanceTypePatternVar,
    };
    use crate::util::{HttpMethodPattern, StringMatcher, ToResolved, ValuePattern};
    use crate::wave::core::cmd::CmdMethod;
    use crate::wave::core::ext::ExtMethod;
    use crate::wave::core::http2::HttpMethod;
    use crate::wave::core::hyp::HypMethod;
    use crate::wave::core::{Method, MethodKind, MethodPattern};


    pub fn result<I: Span, R>(result: Result<(I, R), Err<ErrorTree<I>>>) -> Result<R, SpaceErr> {
        match result {
            Ok((_, e)) => Ok(e),
            Err(err) => Err(find_parse_err(&err)),
        }
    }

    /*
    pub fn just_msg<R, E: From<String>>(
        result: Result<(Span, R), Err<ErrorTree<Span>>>,
    ) -> Result<R, E> {
        match result {
            Ok((_, e)) => Ok(e),
            Err(err) => match find(&err) {
                Ok((message, _)) => Err(E::from(message)),
                Err(err) => Err(E::from(err)),
            },
        }
    }

     */

    fn create_err_report<I: Span>(context: &str, loc: I) -> SpaceErr {
        let mut builder = Report::build(ReportKind::Error, (), 23);

        match NestedBlockKind::error_message(&loc, context) {
            Ok(message) => {
                let builder = builder.with_message(message).with_label(
                    Label::new(loc.location_offset()..loc.location_offset()).with_message(message),
                );
                return ParseErrs::from_report(builder.finish(), loc.extra()).into();
            }
            Err(_) => {}
        }

        let builder = match context {
                "var" => {
                    let f = |input| {preceded(tag("$"),many0(alt((tag("{"),alphanumeric1,tag("-"),tag("_"),multispace1))))(input)};
                    let len = len(f)(loc.clone())+1;
                    builder.with_message("Variables should be lowercase skewer with a leading alphabet character and surrounded by ${} i.e.:'${var-name}' ").with_label(Label::new(loc.location_offset()..loc.location_offset()+len).with_message("Bad Variable Substitution"))
                },
                "assignment:plus" => {
                    builder.with_message("Expecting a preceding '+' (create variable operator)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expected '+'"))
                }
                "assignment:equals" => {
                    builder.with_message("Expecting a preceding '=' for assignment").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting '='"))
                }
                "assignment:value" => {
                    builder.with_message("Expecting a value").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting value"))
                }
                "capture-path" => {
                    builder.with_message("Invalid capture path. Legal characters are filesystem characters plus captures $(var=.*) i.e. /users/$(user=.*)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal capture path"))

                }
                "point" => {
                        builder.with_message("Invalid Point").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Point"))
                    },

                "resolver-not-available" => {
                    builder.with_message("Var & Working Point resolution are not available in this context").with_label(Label::new(loc.location_offset()..loc.location_offset()+loc.len()).with_message("resolution not available"))
                }
                "var-resolver-not-available" => {
                    builder.with_message("Variable resolution is not available in this context").with_label(Label::new(loc.location_offset()..loc.location_offset()+loc.len()).with_message("var resolution not available"))
                }
                "ctx-resolver-not-available" => {
                    builder.with_message("WorkingPoint resolution is not available in this context").with_label(Label::new(loc.location_offset()..loc.location_offset()+loc.len()).with_message("working point resolution not available"))
                }

                "regex" => {
                    let span = result(nospace1(loc.clone()));
                            match span {
                                Ok(span) => {
                                    match Regex::new(loc.to_string().as_str()) {
                                        Ok(_) => {
                                            builder.with_message("internal parse error: regex error in this expression")
                                        }
                                        Err(err) => {
                                            match err {
                                                Error::Syntax(syntax) => {
                                                    builder.with_message(format!("Regex Syntax Error: '{}'",syntax)).with_label(Label::new(span.location_offset()..span.location_offset()+span.len()).with_message("regex syntax error"))
                                                }
                                                Error::CompiledTooBig(size) => {
                                                    builder.with_message("Regex compiled too big").with_label(Label::new(span.location_offset()..span.location_offset()+span.len()).with_message("regex compiled too big"))
                                                }
                                                Error::__Nonexhaustive => {
                                                    builder.with_message("Regex is nonexhaustive").with_label(Label::new(span.location_offset()..span.location_offset()+span.len()).with_message("non-exhaustive regex"))
                                                }
                                            }
                                        }
                                    }
                                }
                        Err(_) => {
                            builder.with_message("internal parse error: could not identify regex")
                        }
                    }
                },
                "expect-camel-case" => { builder.with_message("expecting a CamelCase expression").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting CamelCase"))},
                "expect-skewer-case" => { builder.with_message("expecting a skewer-case expression").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting skewer-case"))},
                "parsed-scopes" => { builder.with_message("expecting a properly formed scope").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("not a scope"))},
                "scope" => { builder.with_message("expecting a properly formed scope").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("not a scope"))},
                "root-scope:block" => { builder.with_message("expecting root scope block {}").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Scope Block"))},
                "pipeline:stop:expecting" =>{ builder.with_message("expecting a pipeline stop: point, call, or return ('&')").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Stop"))},
                "pipeline:step" =>{ builder.with_message("expecting a pipeline step ('->', '=>', '-[ Bin ]->', etc...)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Step"))},
                "pipeline:step:entry" =>{ builder.with_message("expecting a pipeline step entry ('-' or '=') to form a pipeline step i.e. '->' or '=>'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Entry"))},
                "pipeline:step:exit" =>{ builder.with_message("expecting a pipeline step exit i.e. '->' or '=>'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Pipeline Exit"))},
                "pipeline:step:payload" =>{ builder.with_message("Invalid payload filter").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("invalid payload filter"))},
                "scope:expect-space-after-pipeline-step" =>{ builder.with_message("expecting a space after selection pipeline step (->)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Space"))},
                "scope-selector-name:expect-alphanumeric-leading" => { builder.with_message("expecting a valid scope selector name starting with an alphabetic character").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Expecting Alpha Char"))},
                "scope-selector-name:expect-termination" => { builder.with_message("expecting scope selector to be followed by a space, a filter declaration: '(filter)->' or a sub scope selector: '<SubScope> or subscope terminator '>' '").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Bad Scope Selector Termination"))},
                "scope-selector-version-closing-tag" =>{ builder.with_message("expecting a closing parenthesis for the root version declaration (no spaces allowed) -> i.e. Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("missing closing parenthesis"))}
                "scope-selector-version-missing-kazing"=> { builder.with_message("The version declaration needs a little style.  Try adding a '->' to it.  Make sure there are no spaces between the parenthesis and the -> i.e. Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("missing stylish arrow"))}
                "scope-selector-version" => { builder.with_message("Root config selector requires a version declaration with NO SPACES between the name and the version filter example: Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("bad version declaration"))}
                "scope-selector-name" => { builder.with_message("Expecting an alphanumeric scope selector name. example: Pipeline").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting scope selector"))}
                "root-scope-selector-name" => { builder.with_message("Expecting an alphanumeric root scope selector name and version. example: Bind(version=1.0.0)->").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("expecting scope selector"))}
                "consume" => { builder.with_message("Expected to be able to consume the entire String")}
                "point:space_segment:dot_dupes" => { builder.with_message("Space Segment cannot have consecutive dots i.e. '..'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Consecutive dots not allowed"))}
                "point:version:root_not_trailing" =>{ builder.with_message("Root filesystem is the only segment allowed to follow a bundle version i.e. 'space:base:2.0.0-version:/dir/somefile.txt'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Only root file segment ':/' allowed here"))}
                "point:space_segment_leading" => {builder.with_message("The leading character of a Space segment must be a lowercase letter").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Leading Character"))}
                "point:space_segment" => {builder.with_message("A Point Space Segment must be all lowercase, alphanumeric with dashes and dots.  It follows Host and Domain name rules i.e. 'localhost', 'mechtron.io'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Space Segment"))}
                "point:bad_leading" => {builder.with_message("The leading character must be a lowercase letter (for Base Segments) or a digit (for Version Segments)").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Leading Character"))}
                "point:base_segment" => {builder.with_message("A Point Base Segment must be 'skewer-case': all lowercase alphanumeric with dashes. The leading character must be a letter.").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid Base Segment Character"))}
                "point:dir_pop" => {builder.with_message("A Point Directory Pop '..'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Something is Wrong"))}
                "point:dir_segment" => {builder.with_message("A Point Dir Segment follows legal filesystem characters and must end in a '/'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:root_filesystem_segment" => {builder.with_message("Root FileSystem ':/'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:file_segment" => {builder.with_message("A Point File Segment follows legal filesystem characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:file_or_directory"=> {builder.with_message("A Point File Segment (Files & Directories) follows legal filesystem characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "point:version_segment" => {builder.with_message("A Version Segment allows all legal SemVer characters").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Illegal Character"))}
                "filter-name" => {builder.with_message("Filter name must be skewer case with leading character").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid filter name"))}
                "kind-base" => {builder.with_message("Invalid Base Kind").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Base Kind not recognized"))}
            "command" => {builder.with_message("Unrecognized Command").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Invalid"))}
                "parsed-scope-selector-kazing" => {builder.with_message("Selector needs some style with the '->' operator either right after the Selector i.e.: 'Pipeline ->' or as part of the filter declaration i.e. 'Pipeline(auth)->'").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Missing or Invalid Kazing Operator( -> )"))}
                "variable" => {
                        builder.with_message("variable name must be alphanumeric lowercase, dashes and dots.  Variables are preceded by the '$' operator and must be sorounded by curly brackets ${env.valid-variable-name}")
                    },
                "variable:close" => {
                    builder.with_message("variable name must be alphanumeric lowercase, dashes and dots.  Variables are preceded by the '$' operator and must be sorounded by curly brackets with no spaces ${env.valid-variable-name}").with_label(Label::new(loc.location_offset()..loc.location_offset()).with_message("Bad Variable Substitution"))
                },

                "child_perms" => {
                        builder.with_message("expecting child permissions form csd (Create, Select, Delete) uppercase indicates set permission (CSD==full permission, csd==no permission)")
                    },
                    "particle_perms" => {
                        builder.with_message("expecting particle permissions form rwx (Read, Write, Execute) uppercase indicates set permission (RWX==full permission, rwx==no permission)")
                    },
                    "permissions" => {
                        builder.with_message("expecting permissions form 'csd-rwx' (Create,Select,Delete)-(Read,Write,Execute) uppercase indicates set permission (CSD-RWX==full permission, csd-rwx==no permission)")
                    }
                    "permissions_mask" => {
                        builder.with_message("expecting permissions mask symbol '+' for 'Or' mask and '&' for 'And' mask. Example:  &csd-RwX removes ----R-X from current permission")
                    }
                    "privilege" => {
                        builder.with_message("privilege name must be '*' for 'full' privileges or an alphanumeric lowercase, dashes and colons i.e. 'props:email:read'")
                    },
                    "access_grant:perm" => {
                        builder.with_message("expecting permissions mask symbol '+' for 'Or' mask and '&' for 'And' mask. Example:  &csd-RwX removes ----R-X from current permission")
                    },
                    "access_grant:priv" => {
                        builder.with_message("privilege name must be '*' for 'full' privileges or an alphanumeric lowercase, dashes and colons i.e. 'props:email:read'")
                    },
                    "access_grant:on" => {
                        builder.with_message("expecting grant 'on' i.e.: 'grant perm +cSd+RwX on localhost:app:** to localhost:app:users:**<User>'")
                    },
                    "access_grant:to" => {
                        builder.with_message("expecting grant 'to' i.e.: 'grant perm +cSd+RwX on localhost:app:** to localhost:app:users:**<User>'")
                    },
                    "point-subst-brute-force" => {
                        builder.with_message("not expecting variables or working point context '.'/'..' in this point")
                    },
                    "access_grant_kind" => {
                        builder.with_message("expecting access grant kind ['super','perm','priv']")
                    },

                    what => {
                        builder.with_message(format!("internal parser error: cannot determine an error message for parse context: {}",what))
                    }
                };

        //            let source = String::from_utf8(loc.get_line_beginning().to_vec() ).unwrap_or("could not parse utf8 of original source".to_string() );
        ParseErrs::from_report(builder.finish(), loc.extra()).into()
    }
    pub fn find_parse_err<I: Span>(err: &Err<ErrorTree<I>>) -> SpaceErr {
        match err {
            Err::Incomplete(_) => "internal parser error: Incomplete".into(),
            Err::Error(err) => find_tree(err),
            Err::Failure(err) => find_tree(err),
        }
    }

    pub enum ErrFind {
        Context(String),
        Message(String),
    }

    pub fn find_tree<I: Span>(err: &ErrorTree<I>) -> SpaceErr {
        match err {
            ErrorTree::Stack { base, contexts } => {
                let (span, context) = contexts.first().unwrap();
                match context {
                        StackContext::Context(context) => {
                            create_err_report(*context, span.clone())
                        }
                        _ => "internal parser error: could not find a parse context in order to generate a useful error message".into()
                    }
            }
            ErrorTree::Base { location, kind } => create_err_report("eof", location.clone()),
            ErrorTree::Alt(alts) => {
                for alt in alts {
                    return find_tree(alt);
                }

                "internal parser error: ErrorTree::Alt could not find a suitable context error in the various alts".into()
            }
        }
    }

    pub fn first_context<I: Span>(
        orig: Err<ErrorTree<I>>,
    ) -> Result<(String, Err<ErrorTree<I>>), ()> {
        match &orig {
            Err::Error(err) => match err {
                ErrorTree::Stack { base, contexts } => {
                    let (_, context) = contexts.first().unwrap();
                    match context {
                        StackContext::Context(context) => Ok((context.to_string(), orig)),
                        _ => Err(()),
                    }
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

fn inclusive_any_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    alt((tag("+*"), tag("ROOT+*")))(input).map(|(next, _)| (next, PointSegSelector::InclusiveAny))
}

fn inclusive_recursive_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    alt((tag("+**"), tag("ROOT+**")))(input)
        .map(|(next, _)| (next, PointSegSelector::InclusiveRecursive))
}

fn any_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    tag("*")(input).map(|(next, _)| (next, PointSegSelector::Any))
}

fn recursive_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    tag("**")(input).map(|(next, _)| (next, PointSegSelector::Recursive))
}

fn exact_space_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    point_segment_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Space(
                segment.to_string(),
            ))),
        )
    })
}

fn exact_base_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    point_segment_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Base(segment.to_string()))),
        )
    })
}

fn exact_file_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    file_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::File(segment.to_string()))),
        )
    })
}

fn exact_dir_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    file_chars(input).map(|(next, segment)| {
        (
            next,
            PointSegSelector::Exact(ExactPointSeg::PointSeg(PointSeg::Dir(segment.to_string()))),
        )
    })
}

pub fn parse_version_chars_str<I: Span, O: FromStr>(input: I) -> Res<I, O> {
    let (next, rtn) = recognize(version_chars)(input)?;
    match O::from_str(rtn.to_string().as_str()) {
        Ok(rtn) => Ok((next, rtn)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            next,
            ErrorKind::Fail,
        ))),
    }
}

fn exact_version_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    version_req(input).map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
}

fn version_req_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    delimited(tag("("), version_req, tag(")"))(input)
        .map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
}

pub fn point_segment_selector<I: Span>(input: I) -> Res<I, PointSegSelector> {
    alt((
        inclusive_recursive_segment,
        inclusive_any_segment,
        recursive_segment,
        any_segment,
        exact_space_segment,
    ))(input)
}

fn base_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    alt((recursive_segment, any_segment, exact_base_segment))(input)
}

fn file_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    alt((recursive_segment, any_segment, exact_file_segment))(input)
}

fn dir_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    terminated(
        alt((recursive_segment, any_segment, exact_dir_segment)),
        tag("/"),
    )(input)
}

fn dir_segment_meat<I: Span>(input: I) -> Res<I, PointSegSelector> {
    alt((recursive_segment, any_segment, exact_dir_segment))(input)
}

fn version_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    alt((
        recursive_segment,
        any_segment,
        exact_version_segment,
        version_req_segment,
    ))(input)
}

/*
pub fn pattern<'r, O, E: ParseError<&'r str>, V>(
    mut value: V,
) -> impl FnMut(&'r str) -> IResult<&str, Pattern<O>, E>
where
    V: Parser<&'r str, O, E>,
{
    move |input: &str| {
        let x: Res<Span, Span> = tag("*")(input);
        match x {
            Ok((next, _)) => Ok((next, Pattern::Any)),
            Err(_) => {
                let (next, p) = value.parse(input)?;
                let pattern = Pattern::Exact(p);
                Ok((next, pattern))
            }
        }
    }
}

 */
pub fn parse_star_key<I: Span>(input: I) -> Res<I, StarKey> {
    let (next, (_, constelation, _, name, index)) = context(
        "star",
        tuple((
            tag("STAR::"),
            lowercase_alphanumeric,
            tag(":"),
            lowercase_alphanumeric,
            delimited(tag("["), digit1, tag("]")),
        )),
    )(input.clone())?;
    let constelation = constelation.to_string();
    let name = name.to_string();
    let index = match index.to_string().parse::<u16>() {
        Ok(index) => index,
        Err(err) => {
            return Err(nom::Err::Failure(ErrorTree::from_error_kind(
                input,
                ErrorKind::Digit,
            )))
        }
    };

    Ok((
        next,
        StarKey {
            constellation: constelation,
            name,
            index,
        },
    ))
}

pub fn pattern<I: Span, O, E: ParseError<I>, V>(
    mut value: V,
) -> impl FnMut(I) -> IResult<I, Pattern<O>, E>
where
    V: Parser<I, O, E>,
{
    move |input: I| {
        let x: Res<I, I> = tag("*")(input.clone());
        match x {
            Ok((next, _)) => Ok((next, Pattern::Any)),
            Err(_) => {
                let (next, p) = value.parse(input)?;
                let pattern = Pattern::Matches(p);
                Ok((next, pattern))
            }
        }
    }
}

/*
pub fn context<I: Clone, E: ContextError<I>, F, O>(
    context: &'static str,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O, E>
    where
        F: Parser<I, O, E>,
{
    move |i: I| match f.parse(i.clone()) {
        Ok(o) => Ok(o),
        Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
        Err(Err::Error(e)) => Err(Err::Error(E::add_context(i, context, e))),
        Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(i, context, e))),
    }
}

 */
pub fn value_pattern<I: Span, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, ValuePattern<O>, E>
where
    I: InputLength + InputTake + Compare<&'static str>,
    F: Parser<I, O, E>,
    E: nom::error::ContextError<I>,
{
    move |input: I| match tag::<&'static str, I, E>("*")(input.clone()) {
        Ok((next, _)) => Ok((next, ValuePattern::Any)),
        Err(err) => f
            .parse(input.clone())
            .map(|(next, res)| (next, ValuePattern::Pattern(res))),
    }
}
/*
pub fn value_pattern<E,F,O>(
    mut f: F
) -> impl Fn(&str) -> IResult<&str, ValuePattern<O>, E>
where F: Parser<&'static str,O,E>, E: ContextError<&'static str> {
    move |input: &str| match tag::<&str,&'static str,ErrorTree<&'static str>>("*")(input) {
        Ok((next, _)) => Ok((next, ValuePattern::Any)),
        Err(err) => {
            match f.parse(input.clone()) {
                Ok((input,output)) => {Ok((input,ValuePattern::Pattern(output)))}
                Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
                Err(Err::Error(e)) => Err(Err::Error(E::add_context(input.clone(), "value_pattern", e))),
                Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(input.clone(), "value_pattern", e))),
            }
        }
    }
}

 */

/*
pub fn value_pattern<P>(
    parse: fn<I:Span>(input: Span) -> Res<Span, P>,
) -> impl Fn(&str) -> Res<Span, ValuePattern<P>> {
    move |input: &str| match tag::<&str, &str, VerboseError<&str>>("*")(input) {
        Ok((next, _)) => Ok((next, ValuePattern::Any)),
        Err(_) => {
            let (next, p) = parse(input)?;
            let pattern = ValuePattern::Pattern(p);
            Ok((next, pattern))
        }
    }
}
 */

pub fn version_req<I: Span>(input: I) -> Res<I, VersionReq> {
    let (next, version) = version_req_chars(input.clone())?;
    let version = version.to_string();
    let str_input = version.as_str();
    let rtn = semver::VersionReq::parse(str_input);

    match rtn {
        Ok(version) => Ok((next, VersionReq { version })),
        Err(err) => {
            let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
            Err(tree)
        }
    }
}

fn rec_domain<I: Span>(input: I) -> Res<I, I> {
    recognize(tuple((
        many1(terminated(skewer_chars, tag("."))),
        skewer_chars,
    )))(input)
}

// can be a hostname or domain name
fn space<I: Span>(input: I) -> Res<I, I> {
    recognize(alt((skewer_chars, rec_domain)))(input)
}


pub fn rec_domain_pattern<I: Span>(input: I) -> Res<I, Pattern<I>> {
    pattern(rec_domain)(input)
}
pub fn rec_skewer_pattern<I: Span>(input: I) -> Res<I, Pattern<I>> {
    pattern(skewer_chars)(input)
}

pub fn specific_version_req<I: Span>(input: I) -> Res<I, VersionReq> {
    delimited(tag("("), version_req, tag(")"))(input)
}







pub fn to_string<I: Span, F>(mut f: F) -> impl FnMut(I) -> Res<I, String>
where
    F: FnMut(I) -> Res<I, I> + Copy,
{
    move |input: I| {
        f.parse(input)
            .map(|(next, output)| (next, output.to_string()))
    }
}




/*
fn version_req<I:Span>(input: Span) -> Res<Span, VersionReq> {
    let str_input = *input.fragment();
    let rtn:IResult<&str,VersionReq,ErrorTree<&str>> = parse_from_str(version_req_chars).parse(str_input);

    match rtn {
        Ok((next,version_req)) => {
            Ok((span(next), version_req))
        }
        Err(err) => {
            let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
            Err(tree)
        }
    }
}

 */

pub fn version<I: Span>(input: I) -> Res<I, Version> {
    let (next, version) = rec_version(input.clone())?;
    let version = version.to_string();
    let str_input = version.as_str();
    let rtn = semver::Version::parse(str_input);

    match rtn {
        Ok(version) => Ok((next, Version { version })),
        Err(err) => {
            let tree = Err::Error(ErrorTree::from_error_kind(input, ErrorKind::Fail));
            Err(tree)
        }
    }
}



pub fn args<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '"')
                && !(char_item == '_')
                && !(char_item == '{')
                && !(char_item == '}')
                && !(char_item == '(')
                && !(char_item == ')')
                && !(char_item == '[')
                && !(char_item == ']')
                && !(char_item == ' ')
                && !(char_item == '\n')
                && !(char_item == '\t')
                && !(char_item == '\r')
                && !(char_item == '\'')
                && !((char_item.is_alphanumeric()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn skewer<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn skewer_or_snake<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-')
                && !(char_item == '_')
                && !((char_item.is_alpha() && char_item.is_lowercase()) || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn not_quote<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            (char_item == '"')
        },
        ErrorKind::AlphaNumeric,
    )
}

pub fn filename<T>(i: T) -> Res<T, T>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            !(char_item == '-') && !(char_item.is_alpha() || char_item.is_dec_digit())
        },
        ErrorKind::AlphaNumeric,
    )
}



pub fn parse_alpha1_str<I: Span, O: FromStr>(input: I) -> Res<I, O> {
    let (next, rtn) = recognize(alpha1)(input)?;
    match O::from_str(rtn.to_string().as_str()) {
        Ok(rtn) => Ok((next, rtn)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            next,
            ErrorKind::Fail,
        ))),
    }
}

pub fn rc_command<I: Span>(input: I) -> Res<I, CmdKind> {
    parse_alpha1_str(input)
}


pub fn parse_camel_case_str<I: Span, O: FromStr>(input: I) -> Res<I, O> {
    let (next, rtn) = recognize(camel_case_chars)(input)?;
    match O::from_str(rtn.to_string().as_str()) {
        Ok(rtn) => Ok((next, rtn)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            next,
            ErrorKind::Fail,
        ))),
    }
}

pub fn http_method<I: Span>(input: I) -> Res<I, HttpMethod> {
    context("http_method", parse_camel_case_str).parse(input)
}

pub fn http_method_pattern<I: Span>(input: I) -> Res<I, HttpMethodPattern> {
    context("@http_method_pattern", method_pattern(http_method))(input)
}

pub fn method_pattern<I: Clone, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, HttpMethodPattern, E>
where
    I: InputLength + InputTake + Compare<&'static str>,
    F: Parser<I, HttpMethod, E>,
    E: nom::error::ContextError<I>,
{
    move |input: I| match tag::<&'static str, I, E>("*")(input.clone()) {
        Ok((next, _)) => Ok((next, HttpMethodPattern::Any)),
        Err(err) => f
            .parse(input.clone())
            .map(|(next, res)| (next, HttpMethodPattern::Pattern(res))),
    }
}

pub fn ext_method<I: Span>(input: I) -> Res<I, ExtMethod> {
    let (next, ext_method) = camel_case_chars(input.clone())?;

    match ExtMethod::new(ext_method.to_string()) {
        Ok(method) => Ok((next, method)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            input,
            ErrorKind::Fail,
        ))),
    }
}

pub fn sys_method<I: Span>(input: I) -> Res<I, HypMethod> {
    let (next, sys_method) = camel_case_chars(input.clone())?;

    match HypMethod::from_str(sys_method.to_string().as_str()) {
        Ok(method) => Ok((next, method)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            input,
            ErrorKind::Fail,
        ))),
    }
}

pub fn cmd_method<I: Span>(input: I) -> Res<I, CmdMethod> {
    let (next, method) = camel_case_chars(input.clone())?;

    match CmdMethod::from_str(method.to_string().as_str()) {
        Ok(method) => Ok((next, method)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            input,
            ErrorKind::Fail,
        ))),
    }
}

pub fn wrapped_ext_method<I: Span>(input: I) -> Res<I, Method> {
    let (next, ext_method) = ext_method(input.clone())?;

    match ExtMethod::new(ext_method.to_string()) {
        Ok(method) => Ok((next, Method::Ext(method))),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            input,
            ErrorKind::Fail,
        ))),
    }
}

pub fn wrapped_http_method<I: Span>(input: I) -> Res<I, Method> {
    http_method(input).map(|(next, method)| (next, Method::Http(method)))
}

pub fn wrapped_sys_method<I: Span>(input: I) -> Res<I, Method> {
    sys_method(input).map(|(next, method)| (next, Method::Hyp(method)))
}

pub fn wrapped_cmd_method<I: Span>(input: I) -> Res<I, Method> {
    cmd_method(input).map(|(next, method)| (next, Method::Cmd(method)))
}

pub fn rc_command_type<I: Span>(input: I) -> Res<I, CmdKind> {
    parse_alpha1_str(input)
}



fn insert_block_pattern<I: Span>(input: I) -> Res<I, UploadBlock> {
    delimited(multispace0, filename, multispace0)(input).map(|(next, filename)| {
        (
            next,
            UploadBlock {
                name: filename.to_string(),
            },
        )
    })
}

/*
pub fn text_payload_block<I:Span>(input: Span) -> Res<Span, PayloadBlock> {
    delimited(
        tag("+["),
        tuple((
            multispace0,
            delimited(tag("\""), not_quote, tag("\"")),
            multispace0,
        )),
        tag("]"),
    )(input)
    .map(|(next, (_, text, _))| {
        (
            next,
            PayloadBlock::CreatePayload(Payload::Text(text.to_string())),
        )
    })
}*/

pub fn upload_payload_block<I: Span>(input: I) -> Res<I, UploadBlock> {
    delimited(multispace0, file_chars, multispace0)(input).map(|(next, filename)| {
        (
            next,
            UploadBlock {
                name: filename.to_string(),
            },
        )
    })
}

pub fn upload_block<I: Span>(input: I) -> Res<I, UploadBlock> {
    delimited(tag("^["), upload_payload_block, tag("]->"))(input)
}

pub fn upload_blocks<I: Span>(input: I) -> Res<I, Vec<UploadBlock>> {
    many0(pair(take_until("^["), upload_block))(input).map(|(next, blocks)| {
        let mut rtn = vec![];
        for (_, block) in blocks {
            rtn.push(block);
        }
        (next, rtn)
    })
}



/*
pub fn remove_comments_from_span( span: Span )-> Res<Span,Span> {
    let (next,no_comments) = remove_comments(span.clone())?;
    let new = LocatedSpan::new_extra(no_comments.as_str(), span.extra.clone() );
    Ok((next,new))
}
 */

pub fn strip_comments<I: Span>(input: I) -> Res<I, String>
where
    I: InputTakeAtPosition + nom::InputLength + Clone + ToString,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    many0(alt((no_comment, comment)))(input).map(|(next, texts)| {
        let mut rtn = String::new();
        for t in texts {
            match t {
                TextType::NoComment(span) => {
                    rtn.push_str(span.to_string().as_str());
                }
                TextType::Comment(span) => {
                    for i in 0..span.input_len() {
                        // replace with whitespace
                        rtn.push_str(" ");
                    }
                }
            }
        }

        // create with the new string, but use old string as reference
        //let span = LocatedSpan::new_extra(rtn.as_str(), input.extra.clone() );
        (next, rtn)
    })
}

/*
pub fn strip<I:Span>(input: Span) -> Result<Span, ExtErr>
{
    let (_, stripped) = strip_comments(input.clone())?;
    let span = LocatedSpan::new_extra(stripped.as_str().clone(), Arc::new(input.to_string()));
    Ok(span)
}

 */

pub fn no_comment<T: Span>(i: T) -> Res<T, TextType<T>>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item == '#'
        },
        ErrorKind::AlphaNumeric,
    )
    .map(|(next, comment)| (next, TextType::NoComment(comment)))
}

pub fn comment<T: Span>(i: T) -> Res<T, TextType<T>>
where
    T: InputTakeAtPosition + nom::InputLength,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    i.split_at_position1_complete(
        |item| {
            let char_item = item.as_char();
            char_item == '\n'
        },
        ErrorKind::AlphaNumeric,
    )
    .map(|(next, comment)| (next, TextType::Comment(comment)))
}

pub fn bind_config(src: &str) -> Result<BindConfig, SpaceErr> {
    let document = doc(src)?;
    match document {
        Document::BindConfig(bind_config) => Ok(bind_config),
        _ => Err("not a bind config".into()),
    }
}

pub fn mechtron_config(src: &str) -> Result<MechtronConfig, SpaceErr> {
    let document = doc(src)?;
    match document {
        Document::MechtronConfig(mechtron_config) => Ok(mechtron_config),
        _ => Err("not a Mechtron config".into()),
    }
}

pub fn doc(src: &str) -> Result<Document, SpaceErr> {
    let src = src.to_string();
    let (next, stripped) = strip_comments(new_span(src.as_str()))?;
    let span = span_with_extra(stripped.as_str(), Arc::new(src.to_string()));
    let lex_root_scope = lex_root_scope(span.clone())?;
    let root_scope_selector = lex_root_scope.selector.clone().to_concrete()?;
    if root_scope_selector.name.as_str() == "Mechtron" {
        if root_scope_selector.version == Version::from_str("1.0.0")? {
            let mechtron = result(parse_mechtron_config(lex_root_scope.block.content.clone()))?;

            let mechtron = MechtronConfig::new(mechtron)?;
            return Ok(Document::MechtronConfig(mechtron));
        } else {
            let message = format!(
                "ConfigParser does not know how to process a Bind at version '{}'",
                root_scope_selector.version.to_string()
            );
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message(message)
                .with_label(
                    Label::new(
                        lex_root_scope.selector.version.span.location_offset()
                            ..lex_root_scope.selector.version.span.location_offset()
                                + lex_root_scope.selector.version.span.len(),
                    )
                    .with_message("Unsupported Bind Config Version"),
                )
                .finish();
            Err(ParseErrs::from_report(report, lex_root_scope.block.content.extra.clone()).into())
        }
    } else if root_scope_selector.name.as_str() == "Bind" {
        if root_scope_selector.version == Version::from_str("1.0.0")? {
            let bind = parse_bind_config(lex_root_scope.block.content.clone())?;

            return Ok(Document::BindConfig(bind));
        } else {
            let message = format!(
                "ConfigParser does not know how to process a Bind at version '{}'",
                root_scope_selector.version.to_string()
            );
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message(message)
                .with_label(
                    Label::new(
                        lex_root_scope.selector.version.span.location_offset()
                            ..lex_root_scope.selector.version.span.location_offset()
                                + lex_root_scope.selector.version.span.len(),
                    )
                    .with_message("Unsupported Bind Config Version"),
                )
                .finish();
            Err(ParseErrs::from_report(report, lex_root_scope.block.content.extra.clone()).into())
        }
    } else {
        let message = format!(
            "ConfigParser does not know how to process a '{}'",
            lex_root_scope.selector.name.to_string(),
        );
        let mut builder = Report::build(ReportKind::Error, (), 0);
        let report = builder
            .with_message(message)
            .with_label(
                Label::new(
                    lex_root_scope.selector.name.location_offset()
                        ..lex_root_scope.selector.name.location_offset()
                            + lex_root_scope.selector.name.len(),
                )
                .with_message("Unrecognized Config Kind"),
            )
            .finish();
        Err(ParseErrs::from_report(report, lex_root_scope.block.content.extra.clone()).into())
    }
}

fn parse_mechtron_config<I: Span>(input: I) -> Res<I,Vec<MechtronScope>> {
    let (next, (_,( _, (_,assignments)))) = pair( multispace0, context("wasm",tuple((
        tag("Wasm"),
        alt((tuple((
        multispace0,
            unwrap_block(BlockKind::Nested(NestedBlockKind::Curly),many0(assignment)))
        ),fail))),
    )))(input)?;
    Ok((next,vec![MechtronScope::WasmScope(assignments)]))
}

fn assignment<I>(input: I) -> Res<I, Assignment>
where
    I: Span,
{
    tuple((
        multispace0,
        context("assignment:plus", alt( (tag("+"),fail))),
        context("assignment:key", alt( (skewer,fail))),
        multispace0,
        context( "assignment:equals", alt((tag("="),fail))),
        multispace0,
        context( "assignment:value", alt((nospace1_nosemi,fail))),
        multispace0,
        opt(tag(";")),
        multispace0
    ))(input)
    .map(|(next, (_, _, k, _, _, _, v, _, _, _))| {
        (
            next,
            Assignment {
                key: k.to_string(),
                value: v.to_string(),
            },
        )
    })
}

#[derive(Clone)]
pub struct Assignment {
    pub key: String,
    pub value: String,
}

fn semantic_mechtron_scope<I: Span>(scope: LexScope<I>) -> Result<MechtronScope, SpaceErr> {
    let selector_name = scope.selector.name.to_string();
    match selector_name.as_str() {
        "Wasm" => {
            let assignments = result(many0(assignment)(scope.block.content))?;
            Ok(MechtronScope::WasmScope(assignments))
        }
        what => {
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message(format!(
                    "Unrecognized MechtronConfig selector: '{}'",
                    scope.selector.name.to_string()
                ))
                .with_label(
                    Label::new(
                        scope.selector.name.location_offset()
                            ..scope.selector.name.location_offset() + scope.selector.name.len(),
                    )
                    .with_message("Unrecognized Selector"),
                )
                .finish();
            Err(ParseErrs::from_report(report, scope.block.content.extra().clone()).into())
        }
    }
}

fn parse_bind_config<I: Span>(input: I) -> Result<BindConfig, SpaceErr> {
    let lex_scopes = lex_scopes(input)?;
    let mut scopes = vec![];
    let mut errors = vec![];

    for lex_scope in lex_scopes {
        match semantic_bind_scope(lex_scope) {
            Ok(scope) => {
                scopes.push(scope);
            }
            Err(err) => errors.push(err),
        }
    }

    if !errors.is_empty() {
        let errors = ParseErrs::fold(errors);
        return Err(errors.into());
    }

    let mut config = BindConfig::new(scopes);
    Ok(config)
}

fn semantic_bind_scope<I: Span>(scope: LexScope<I>) -> Result<BindScope, SpaceErr> {
    let selector_name = scope.selector.name.to_string();
    match selector_name.as_str() {
        "Route" => {
            let scope = lex_child_scopes(scope)?;
            let scope = RouteScope::try_from(scope)?;
            Ok(BindScope::RequestScope(scope))
        }
        what => {
            let mut builder = Report::build(ReportKind::Error, (), 0);
            let report = builder
                .with_message(format!(
                    "Unrecognized BindConfig selector: '{}'",
                    scope.selector.name.to_string()
                ))
                .with_label(
                    Label::new(
                        scope.selector.name.location_offset()
                            ..scope.selector.name.location_offset() + scope.selector.name.len(),
                    )
                    .with_message("Unrecognized Selector"),
                )
                .finish();
            Err(ParseErrs::from_report(report, scope.block.content.extra().clone()).into())
        }
    }
}

fn parse_bind_pipelines_scope<I: Span>(input: I) -> Result<Spanned<I, BindScopeKind>, ParseErrs> {
    unimplemented!()
    /*
    let (next, lex_scopes) = lex_scopes(input.clone())?;
    let mut errs = vec![];
    for lex_scope in lex_scopes {
        match lex_scope.selector.name.to_string().as_str() {
            "Ext" => {}
            "Http" => {}
            "Rc" => {}
            what => {
                let mut builder = Report::build(ReportKind::Error, (), 0);
                let report = builder
                    .with_message(format!("Unrecognized Pipeline scope: '{}'", what))
                    .with_label(
                        Label::new(input.location_offset()..input.location_offset())
                            .with_message("Unrecognized Selector"),
                    )
                    .finish();
                errs.push(ParseErrs::new(report, input.extra.clone()));
            }
        }
    }

    if !errs.is_empty() {
        Err(ParseErrs::fold(errs))
    } else {
        Ok(ElemSpan::new(BindBlock::Pipelines, input.clone()))
    }

     */
}

pub fn nospace0<I: Span>(input: I) -> Res<I, I> {
    recognize(many0(satisfy(|c| !c.is_whitespace())))(input)
}

pub fn nospace1<I: Span>(input: I) -> Res<I, I> {
    recognize(pair(
        satisfy(|c| !c.is_whitespace()),
        many0(satisfy(|c| !c.is_whitespace())),
    ))(input)
}
pub fn nospace1_nosemi<I: Span>(input: I) -> Res<I, I> {
    recognize(pair(
        satisfy(|c| !c.is_whitespace() && ';' != c),
        many0(satisfy(|c| !c.is_whitespace( ) && ';' != c)),
    ))(input)
}



pub fn no_space_with_blocks<I: Span>(input: I) -> Res<I, I> {
    recognize(many1(alt((recognize(any_block), nospace1))))(input)
}

pub fn pipeline_step_var<I: Span>(input: I) -> Res<I, PipelineStepVar> {
    context(
        "pipeline:step",
        tuple((
            alt((
                value(WaveDirection::Direct, tag("-")),
                value(WaveDirection::Reflect, tag("=")),
            )),
            opt(pair(
                delimited(
                    tag("["),
                    context("pipeline:step:exit", alpha0),
                    tag("]"),
                ),
                context(
                    "pipeline:step:payload",
                    cut(alt((
                        value(WaveDirection::Direct, tag("-")),
                        value(WaveDirection::Reflect, tag("=")),
                    ))),
                ),
            )),
            context("pipeline:step:exit", cut(tag(">"))),
        )),
    )(input)
    .map(|(next, (entry, block_and_exit, _))| {
 //       let mut blocks = vec![];
        let exit = match block_and_exit {
            None => entry.clone(),
            Some((block, exit)) => {
//                blocks.push(block);
                exit
            }
        };

        (
            next,
            PipelineStepVar {
                entry,
                exit,
                blocks: vec![],
            },
        )
    })
}

pub fn core_pipeline_stop<I: Span>(input: I) -> Res<I, PipelineStopVar> {
    context(
        "Core",
        delimited(
            tag("(("),
            delimited(multispace0, opt(tag("*")), multispace0),
            tag("))"),
        ),
    )(input)
    .map(|(next, _)| (next, PipelineStopVar::Core))
}

pub fn return_pipeline_stop<I: Span>(input: I) -> Res<I, PipelineStopVar> {
    tag("&")(input).map(|(next, _)| (next, PipelineStopVar::Reflect))
}


pub fn point_pipeline_stop<I: Span>(input: I) -> Res<I, PipelineStopVar> {
    context("pipeline:stop:point", point_var)(input)
        .map(|(next, point)| (next, PipelineStopVar::Point(point)))
}

pub fn pipeline_stop_var<I: Span>(input: I) -> Res<I, PipelineStopVar> {
    context(
        "Stop",
        pair(
            context(
                "pipeline:stop:expecting",
                cut(peek(alt((tag("(("), tag("."), alpha1, tag("&"))))),
            ),
            alt((
                core_pipeline_stop,
                return_pipeline_stop,
                point_pipeline_stop,
            )),
        ),
    )(input)
    .map(|(next, (_, pipeline_stop))| (next, pipeline_stop))
}

pub fn consume_pipeline_step<I: Span>(input: I) -> Res<I, PipelineStepVar> {
    all_consuming(pipeline_step_var)(input)
}

pub fn consume_pipeline_stop<I: Span>(input: I) -> Res<I, PipelineStopVar> {
    all_consuming(pipeline_stop_var)(input)
}

pub fn pipeline_segment<I: Span>(input: I) -> Res<I, PipelineSegmentVar> {
    tuple((
        multispace0,
        pipeline_step_var,
        multispace0,
        pipeline_stop_var,
        multispace0,
    ))(input)
    .map(|(next, (_, step, _, stop, _))| (next, PipelineSegmentVar { step, stop }))
}

pub fn pipeline<I: Span>(input: I) -> Res<I, PipelineVar> {
    context(
        "pipeline",
        many0(delimited(multispace0, pipeline_segment, multispace0)),
    )(input)
    .map(|(next, segments)| (next, PipelineVar { segments }))
}

pub fn consume_pipeline<I: Span>(input: I) -> Res<I, PipelineVar> {
    all_consuming(pipeline)(input)
}

/*
pub fn entity_selectors<I:Span>(input: Span) -> Res<Span, Vec<Selector<PipelineSelector>>> {
    many0(delimited(multispace0, entity_selector, multispace0))(input)
}

pub fn entity_selector<I:Span>(input: Span) -> Res<Span, Selector<PipelineSelector>> {
    tuple((entity_pattern, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn msg_selector<I:Span>(input: Span) -> Res<Span, Selector<ExtPipelineSelector>> {
    tuple((msg_pattern_scoped, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn http_pipeline<I:Span>(input: Span) -> Res<Span, Selector<HttpPipelineSelector>> {
    tuple((http_pattern_scoped, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn rc_selector<I:Span>(input: Span) -> Res<Span, Selector<RcPipelineSelector>> {
    tuple((rc_pattern_scoped, multispace0, pipeline, tag(";")))(input)
        .map(|(next, (pattern, _, pipeline, _))| (next, Selector::new(pattern, pipeline)))
}

pub fn consume_selector<I:Span>(input: Span) -> Res<Span, Selector<PipelineSelector>> {
    all_consuming(entity_selector)(input)
}

 */

pub fn subst<I: Span, F>(f: F) -> impl FnMut(I) -> Res<I, Subst<I>>
where
    F: FnMut(I) -> Res<I, I> + Copy,
{
    move |input: I| {
        many1(chunk(f))(input.clone()).map(|(next, chunks)| {
            let len: usize = chunks.iter().map(|c| c.len()).sum();
            let span = input.slice(0..input.len() - next.len());
            let chunks = Subst {
                chunks,
                trace: span.trace(),
            };
            (next, chunks)
        })
    }
}

pub fn chunk<I: Span, F>(mut f: F) -> impl FnMut(I) -> Res<I, Chunk<I>> + Copy
where
    F: FnMut(I) -> Res<I, I> + Copy,
{
    move |input: I| alt((var_chunk, text_chunk(f)))(input)
}

pub fn text_chunk<I: Span, F>(mut f: F) -> impl FnMut(I) -> Res<I, Chunk<I>> + Copy
where
    F: FnMut(I) -> Res<I, I> + Copy,
{
    move |input: I| f(input).map(|(next, text)| (next, Chunk::Text(text)))
}

pub fn var_chunk<I: Span>(input: I) -> Res<I, Chunk<I>> {
    preceded(
        tag("$"),
        context(
            "variable",
            cut(delimited(
                context("variable:open", cut(tag("{"))),
                context("variable:name", variable_name),
                context("variable:close", cut(tag("}"))),
            )),
        ),
    )(input)
    .map(|(next, variable_name)| (next, Chunk::Var(variable_name)))
}
/*
pub fn unwrap_route_selector(input: &str ) -> Result<RouteSelector,ExtErr> {
    let input = new_span(input);
    let input = result(unwrap_block( BlockKind::Nested(NestedBlockKind::Parens),input))?;
}

 */
pub fn route_attribute(input: &str) -> Result<RouteSelector, SpaceErr> {
    let input = new_span(input);
    let (_, (_, lex_route)) = result(pair(
        tag("#"),
        unwrap_block(
            BlockKind::Nested(NestedBlockKind::Square),
            pair(
                tag("route"),
                unwrap_block(
                    BlockKind::Nested(NestedBlockKind::Parens),
                    unwrap_block(
                        BlockKind::Delimited(DelimitedBlockKind::DoubleQuotes),
                        nospace0,
                    ),
                ),
            ),
        ),
    )(input.clone()))?;

    route_selector(lex_route)
}

pub fn route_attribute_value(input: &str) -> Result<RouteSelector, SpaceErr> {
    let input = new_span(input);
    let lex_route = result(unwrap_block(
        BlockKind::Delimited(DelimitedBlockKind::DoubleQuotes),
        trim(nospace0),
    )(input.clone()))?;

    route_selector(lex_route)
}

/*
pub fn topic<I: Span>(input: I) -> Res<I, ValuePattern<Topic>> {
    context(
        "topic",
        delimited(tag("["), value_pattern(skewer_case_chars), tag("]::")),
    )(input)
    .map(|(next, topic)| {
        let topic = match topic {
            ValuePattern::Any => ValuePattern::Any,
            ValuePattern::None => ValuePattern::None,
            ValuePattern::Pattern(topic) => ValuePattern::Pattern(Topic::Tag(topic.to_string())),
        };
        (next, topic)
    })
}

 */

pub fn route_selector<I: Span>(input: I) -> Result<RouteSelector, SpaceErr> {
    let (next, (topic, lex_route)) = match pair(
        opt(terminated(
            unwrap_block(
                BlockKind::Nested(NestedBlockKind::Square),
                value_pattern(topic),
            ),
            tag("::"),
        )),
        lex_route_selector,
    )(input.clone())
    {
        Ok((next, (topic, lex_route))) => (next, (topic, lex_route)),
        Err(err) => {
            return Err(find_parse_err(&err));
        }
    };

    if next.len() > 0 {
        return Err(ParseErrs::from_loc_span(
            "could not consume entire route selector",
            "extra",
            next,
        )
        .into());
    }

    let mut names = lex_route.names.clone();
    names.reverse();
    let method_kind_span = names
        .pop()
        .ok_or(ParseErrs::from_loc_span(
            "expecting MethodKind [ Http, Ext ]",
            "expecting MethodKind",
            input,
        ))?
        .clone();
    let method_kind = result(value_pattern(method_kind)(method_kind_span.clone()))?;
    let method = match &method_kind {
        ValuePattern::Any => ValuePattern::Any,
        ValuePattern::None => ValuePattern::None,
        ValuePattern::Pattern(method_kind) => match method_kind {
            MethodKind::Hyp => {
                let method = names.pop().ok_or(ParseErrs::from_loc_span(
                    "Hyp method requires a sub kind i.e. Hyp<Assign> or Ext<*>",
                    "sub kind required",
                    method_kind_span,
                ))?;
                let method = result(value_pattern(sys_method)(method))?;
                ValuePattern::Pattern(MethodPattern::Hyp(method))
            }
            MethodKind::Cmd => {
                let method = names.pop().ok_or(ParseErrs::from_loc_span(
                    "Cmd method requires a sub kind i.e. Cmd<Bounce>",
                    "sub kind required",
                    method_kind_span,
                ))?;
                let method = result(value_pattern(cmd_method)(method))?;
                ValuePattern::Pattern(MethodPattern::Cmd(method))
            }
            MethodKind::Ext => {
                let method = names.pop().ok_or(ParseErrs::from_loc_span(
                    "Ext method requires a sub kind i.e. Ext<SomeExt> or Ext<*>",
                    "sub kind required",
                    method_kind_span,
                ))?;
                let method = result(value_pattern(ext_method)(method))?;
                ValuePattern::Pattern(MethodPattern::Ext(method))
            }
            MethodKind::Http => {
                let method = names.pop().ok_or(ParseErrs::from_loc_span(
                    "Http method requires a sub kind i.e. Http<Get> or Http<*>",
                    "sub kind required",
                    method_kind_span,
                ))?;
                let method = result(value_pattern(http_method)(method))?;
                ValuePattern::Pattern(MethodPattern::Http(method))
            }
        },
    };

    if !names.is_empty() {
        let name = names.pop().unwrap();
        return Err(ParseErrs::from_loc_span("Too many SubKinds: only Http/Ext supported with one subkind i.e. Http<Get>, Ext<MyMethod>", "too many subkinds", name).into());
    }

    let path = match lex_route.path.as_ref() {
        None => Regex::new("/.*").unwrap(),
        Some(i) => match Regex::new(i.to_string().as_str()) {
            Ok(path) => path,
            Err(err) => {
                return Err(ParseErrs::from_loc_span(
                    format!("cannot parse Path regex: '{}'", err.to_string()).as_str(),
                    "path regex error",
                    i.clone(),
                ));
            }
        },
    };

    Ok(RouteSelector::new(
        topic,
        method,
        path,
        lex_route.filters.to_scope_filters(),
    ))
}

fn create_command<I: Span>(input: I) -> Res<I, CommandVar> {
    tuple((tag("create"), create))(input)
        .map(|(next, (_, create))| (next, CommandVar::Create(create)))
}

pub fn publish_command<I: Span>(input: I) -> Res<I, CommandVar> {
    tuple((tag("publish"), space1, publish))(input)
        .map(|(next, (_, _, create))| (next, CommandVar::Create(create)))
}

fn select_command<I: Span>(input: I) -> Res<I, CommandVar> {
    tuple((tag("select"), space1, select))(input)
        .map(|(next, (_, _, select))| (next, CommandVar::Select(select)))
}

fn set_command<I: Span>(input: I) -> Res<I, CommandVar> {
    tuple((tag("set"), space1, set))(input).map(|(next, (_, _, set))| (next, CommandVar::Set(set)))
}

fn get_command<I: Span>(input: I) -> Res<I, CommandVar> {
    tuple((tag("get"), space1, get))(input).map(|(next, (_, _, get))| (next, CommandVar::Get(get)))
}

pub fn command_strategy<I: Span>(input: I) -> Res<I, Strategy> {
    opt(tuple((tag("?"), multispace0)))(input).map(|(next, hint)| match hint {
        None => (next, Strategy::Commit),
        Some(_) => (next, Strategy::Ensure),
    })
}

pub fn command<I: Span>(input: I) -> Res<I, CommandVar> {
    context(
        "command",
        alt((
            create_command,
            publish_command,
            select_command,
            set_command,
            get_command,
            fail,
        )),
    )(input)
}

pub fn command_line<I: Span>(input: I) -> Res<I, CommandVar> {
    tuple((
        multispace0,
        command,
        multispace0,
        opt(tag(";")),
        multispace0,
    ))(input)
    .map(|(next, (_, command, _, _, _))| (next, command))
}

pub fn script_line<I: Span>(input: I) -> Res<I, CommandVar> {
    tuple((multispace0, command, multispace0, tag(";"), multispace0))(input)
        .map(|(next, (_, command, _, _, _))| (next, command))
}

pub fn script<I: Span>(input: I) -> Res<I, Vec<CommandVar>> {
    many0(script_line)(input)
}

pub fn consume_command_line<I: Span>(input: I) -> Res<I, CommandVar> {
    all_consuming(command_line)(input)
}

pub fn rec_script_line<I: Span>(input: I) -> Res<I, I> {
    recognize(script_line)(input)
}

#[cfg(test)]
pub mod cmd_test {
    use core::str::FromStr;

    use nom::error::{VerboseError, VerboseErrorKind};
    use nom_supreme::final_parser::{ExtractContext, final_parser};

    use cosmic_nom::{new_span, Res};

    use crate::command::{Command, CommandVar};
    use crate::err::SpaceErr;
    use crate::parse::error::result;
    use crate::parse::{
        command, create_command, publish_command, script, upload_blocks,
    };
    use crate::util::ToResolved;
    use crate::{ SetProperties};
    use crate::model::CamelCase;

    /*
    #[mem]
    pub async fn test2() -> Result<(),Error>{
        let input = "? xreate localhost<Space>";
        let x: Result<CommandOp,VerboseError<&str>> = final_parser(command)(input);
        match x {
            Ok(_) => {}
            Err(err) => {
                println!("err: {}", err.to_string())
            }
        }


        Ok(())
    }

     */

    //    #[test]
    pub fn test() -> Result<(), SpaceErr> {
        let input = "xreate? localhost<Space>";
        match command(new_span(input)) {
            Ok(_) => {}
            Err(nom::Err::Error(e)) => {
                eprintln!("yikes!");
                return Err("could not find context".into());
            }
            Err(e) => {
                return Err("some err".into());
            }
        }
        Ok(())
    }



    #[test]
    pub fn test_script() -> Result<(), SpaceErr> {
        let input = r#" create? localhost<Space>;
 Xcrete localhost:repo<Base<Repo>>;
 create? localhost:repo:tutorial<ArtifactBundleSeries>;
 publish? ^[ bundle.zip ]-> localhost:repo:tutorial:1.0.0;
 set localhost{ +bind=localhost:repo:tutorial:1.0.0:/bind/localhost.bind } ;
        "#;

        crate::parse::script(new_span(input))?;
        Ok(())
    }

    #[test]
    pub fn test_publish() -> Result<(), SpaceErr> {
        let input = r#"publish ^[ bundle.zip ]-> localhost:repo:tutorial:1.0.0"#;
        publish_command(new_span(input))?;
        Ok(())
    }

    #[test]
    pub fn test_upload_blocks() -> Result<(), SpaceErr> {
        let input = r#"publish ^[ bundle.zip ]-> localhost:repo:tutorial:1.0.0"#;
        let blocks = result(upload_blocks(new_span(input)))?;
        assert_eq!(1, blocks.len());
        let block = blocks.get(0).unwrap();
        assert_eq!("bundle.zip", block.name.as_str());

        // this should fail bcause it has multiple ^[
        let input = r#"publish ^[ ^[ bundle.zip ]-> localhost:repo:tutorial:1.0.0"#;
        let blocks = result(upload_blocks(new_span(input)))?;
        assert_eq!(0, blocks.len());

        // this should fail bcause it has no ^[
        let input = r#"publish localhost:repo:tutorial:1.0.0"#;
        let blocks = result(upload_blocks(new_span(input)))?;
        assert_eq!(0, blocks.len());

        Ok(())
    }


    #[test]
    pub fn test_create_properties() -> Result<(), SpaceErr> {
        let input = r#"create localhost:repo:tutorial:1.0.0<Repo>{ +config=the:cool:property }"#;
        let mut command = result(create_command(new_span(input)))?;
        let command = command.collapse()?;
        if let Command::Create(create) = command {
            assert!(create.properties.get("config").is_some());
        } else {
            assert!(false);
        }

        Ok(())
    }
}

pub fn layer<I: Span>(input: I) -> Res<I, Layer> {
    let (next, layer) = recognize(camel_case)(input.clone())?;
    match Layer::from_str(layer.to_string().as_str()) {
        Ok(layer) => Ok((next, layer)),
        Err(err) => Err(nom::Err::Error(ErrorTree::from_error_kind(
            input,
            ErrorKind::Alpha,
        ))),
    }
}

fn topic_uuid<I: Span>(input: I) -> Res<I, Topic> {
    delimited(tag("Topic<Uuid>("), parse_uuid, tag(")"))(input)
        .map(|(next, uuid)| ((next, Topic::Uuid(uuid))))
}

fn topic_cli<I: Span>(input: I) -> Res<I, Topic> {
    value(Topic::Cli, tag("Topic<Cli>"))(input)
}

fn topic_path<I: Span>(input: I) -> Res<I, Topic> {
    delimited(tag("Topic<Path>("), many1(skewer_case), tag(")"))(input)
        .map(|(next, segments)| ((next, Topic::Path(segments))))
}

fn topic_any<I: Span>(input: I) -> Res<I, Topic> {
    context("Topic<*>", value(Topic::Any, tag("Topic<*>")))(input)
}

fn topic_not<I: Span>(input: I) -> Res<I, Topic> {
    context("Topic<Not>", value(Topic::Not, tag("Topic<!>")))(input)
}

pub fn topic_none<I: Span>(input: I) -> Res<I, Topic> {
    Ok((input, Topic::None))
}

pub fn topic<I: Span>(input: I) -> Res<I, Topic> {
    context(
        "topic",
        alt((topic_cli, topic_path, topic_uuid, topic_any, topic_not)),
    )(input)
}

pub fn topic_or_none<I: Span>(input: I) -> Res<I, Topic> {
    context("topic_or_none", alt((topic, topic_none)))(input)
}

pub fn plus_topic_or_none<I: Span>(input: I) -> Res<I, Topic> {
    context(
        "plus_topic_or_none",
        alt((preceded(tag("+"), topic), topic_none)),
    )(input)
}

pub fn port<I: Span>(input: I) -> Res<I, Surface> {
    let (next, (point, layer, topic)) = context(
        "port",
        tuple((
            terminated(tw(point_var), tag("@")),
            layer,
            plus_topic_or_none,
        )),
    )(input.clone())?;

    match point.w.collapse() {
        Ok(point) => Ok((next, Surface::new(point, layer, topic))),
        Err(err) => {
            let err = ErrorTree::from_error_kind(input.clone(), ErrorKind::Alpha);
            let loc = input.slice(point.trace.range);
            Err(nom::Err::Error(ErrorTree::add_context(
                loc,
                "resolver-not-available",
                err,
            )))
        }
    }
}


pub fn expect<I,O,F>( mut f: F ) -> impl FnMut(I) -> Res<I,O> where F: FnMut(I) -> Res<I,O>+Copy{
    move |i: I| {
        f(i).map_err( |e| {
            match e {
                Err::Incomplete(i) => {
                    Err::Incomplete(i)
                }
                Err::Error(e) => {
                    Err::Failure(e)
                }
                Err::Failure(e) => {
                    Err::Failure(e)
                }
            }
        })
    }
}