use crate::parse::{
    base_segment, file_chars, point_route_segment,  skewer, skewer_case,
    var_route, version, version_req,
};
use crate::point::{Point, PointDef, PointSeg, PointSegment, RouteSeg};
use crate::selector::{ExactPointSeg, PointSegSelector, PointSelector};
use cosmic_nom::{Res, Span};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{opt, peek, value};
use nom::error::context;
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

pub fn point_def<I, Route, Segment, FnRoute, FnBaseSegment, FnFileSysSegment>(
    fn_route: FnRoute,
    fn_segment: FnBaseSegment,
    fn_fs_segment: FnFileSysSegment,
) -> impl FnMut(I) -> Res<I, PointDef<Route, Segment>>
where
    I: Span,
    FnRoute: FnMut(I) -> Res<I, Route> + Copy,
    FnBaseSegment: FnMut(I) -> Res<I, Segment> + Copy,
    FnFileSysSegment: FnMut(I) -> Res<I, Segment> + Copy,
    Route: Default,
    Segment: PointSegment,
{
    move |input: I| {
        tuple((
            opt(terminated(fn_route, tag("::"))),
            separated_list1(tag(":"), fn_segment),
            opt(preceded(tag(":/"), many0(fn_fs_segment))),
        ))(input)
        .map(|(next, (route, mut base_segments, mut fs_segments))| {
            let mut segments = vec![];

            segments.append(&mut base_segments);

            if let Some(mut fs_segments) = fs_segments {
                segments.push(Segment::file_sys_root());
                segments.append(&mut fs_segments)
            }

            let route = match route {
                Some(route) => route,
                None => Route::default(),
            };

            (next, PointDef { route, segments })
        })
    }
}

fn point_segment<I>(input: I) -> Res<I, PointSeg>
where
    I: Span,
{
    context(
        "point-segment",
        alt((base_point_segment, version_point_segment)),
    )(input)
}

fn point_segment_selector<I>(input: I) -> Res<I, PointSegSelector>
where
    I: Span,
{
    context(
        "point-segment-selector",
        alt((
            value(PointSegSelector::InclusiveAny, tag("+:*")),
            value(PointSegSelector::InclusiveRecursive, tag("+:**")),
            value(PointSegSelector::Recursive, tag("**")),
            value(PointSegSelector::Any, tag("*")),
            base_point_segment_selector,
            version_req_segment,
        )),
    )(input)
}

fn file_segment_selector<I>(input: I) -> Res<I, PointSegSelector>
where
    I: Span,
{
    context(
        "point-segment-selector",
        alt((
            value(PointSegSelector::InclusiveAny, tag("+:*")),
            value(PointSegSelector::InclusiveRecursive, tag("+:**")),
            value(PointSegSelector::Recursive, tag("**")),
            value(PointSegSelector::Any, tag("*")),
            filesys_point_segment_selector,
            version_req_segment,
        )),
    )(input)
}

fn base_point_segment_selector<I: Span>(input: I) -> Res<I, PointSegSelector> {
    base_point_segment(input)
        .map(|(next, base)| (next, PointSegSelector::Exact(ExactPointSeg::PointSeg(base))))
}

fn filesys_point_segment_selector<I: Span>(input: I) -> Res<I, PointSegSelector> {
    filesys_point_segment(input)
        .map(|(next, base)| (next, PointSegSelector::Exact(ExactPointSeg::PointSeg(base))))
}

fn version_req_segment<I: Span>(input: I) -> Res<I, PointSegSelector> {
    delimited(tag("("), version_req, tag(")"))(input)
        .map(|(next, version_req)| (next, PointSegSelector::Version(version_req)))
}
fn base_point_segment<I>(input: I) -> Res<I, PointSeg>
where
    I: Span,
{
    skewer_case(input).map(|(next, skewer)| (next, PointSeg::Base(skewer.to_string())))
}

fn version_point_segment<I>(input: I) -> Res<I, PointSeg>
where
    I: Span,
{
    version(input).map(|(next, version)| (next, PointSeg::Version(version)))
}

fn filesys_point_segment<I>(input: I) -> Res<I, PointSeg>
where
    I: Span,
{
    context(
        "point-segment-filesys",
        alt((dir_point_segment, file_point_segment)),
    )(input)
}

fn dir_point_segment<I>(input: I) -> Res<I, PointSeg>
where
    I: Span,
{
    terminated(file_chars, tag("/"))(input)
        .map(|(next, dir)| (next, PointSeg::Dir(format!("{}/", dir.to_string()))))
}

fn file_point_segment<I>(input: I) -> Res<I, PointSeg>
where
    I: Span,
{
    file_chars(input).map(|(next, file)| (next, PointSeg::File(file.to_string())))
}

pub fn point<I>(input: I) -> Res<I, Point>
where
    I: Span,
{
    point_def(point_route_segment, point_segment, filesys_point_segment)(input)
}

pub fn point_selector<I>(input: I) -> Res<I, PointSelector>
where
    I: Span,
{
    point_def(
        point_route_segment,
        point_segment_selector,
        file_segment_selector,
    )(input)
}

#[cfg(test)]
pub mod test {
    use crate::parse::error::result;
    use crate::parse::point::{point, point_selector};
    use crate::parse::skewer_case;
    use crate::point::{Point, PointSeg};
    use cosmic_nom::new_span;
    use nom::combinator::all_consuming;
    use crate::selector::PointSelector;
    use crate::util::log;

    #[test]
    pub fn test_point() {
        all_consuming(point)(new_span("localhost")).unwrap();
        all_consuming(point)(new_span("localhost:some")).unwrap();
        all_consuming(point)(new_span("localhost:some:1.0.0")).unwrap();
        all_consuming(point)(new_span("localhost:some:/dir/")).unwrap();
        all_consuming(point)(new_span("localhost:some:/dir/file.txt")).unwrap();

        let skewer = result(skewer_case(new_span("xyzBad"))).unwrap();

        assert_eq!(skewer.to_string().as_str(), "xyz");
    }


    #[test]
    pub fn test_selector() {
        log(result(all_consuming(point_selector)(new_span("localhost")))).unwrap();
        log(result(all_consuming(point_selector)(new_span("localhost:*")))).unwrap();
        log(result(all_consuming(point_selector)(new_span("localhost:*:bye")))).unwrap();
        log(result(all_consuming(point_selector)(new_span("localhost:**:(^1.0.0)")))).unwrap();
        log(result(all_consuming(point_selector)(new_span("localhost:some:/dir/file.txt")))).unwrap();
        log(result(all_consuming(point_selector)(new_span("localhost:some:/dir/*")))).unwrap();
    }

    #[test]
    pub fn test_selector2() {
        {
            let p1= result(all_consuming(point)(new_span("localhost"))).unwrap();
            let p2= result(all_consuming(point)(new_span("otherhost"))).unwrap();
            let selector : PointSelector= log(result(all_consuming(point_selector)(new_span("localhost")))).unwrap();
            assert!(selector.is_match(&p1));
            assert!(!selector.is_match(&p2));
        }
    }


}
