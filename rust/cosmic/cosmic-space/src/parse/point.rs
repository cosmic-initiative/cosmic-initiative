use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{opt, peek};
use nom::error::context;
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{pair, preceded, terminated, tuple};
use crate::point::{Point, PointDef, PointSeg, PointSegment, RouteSeg};
use cosmic_nom::{Res, Span};
use crate::parse::{base_segment, file_chars, point_route_segment, skewer, skewer_case, var_route, version};

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
    Segment: PointSegment
{
    move |input: I| {
       tuple((
           opt(terminated(fn_route,tag("::"))),
          separated_list1(tag(":"), fn_segment),
           opt( preceded(tag(":/"),
       separated_list0(tag("/"), fn_fs_segment )))))(input).map(|(next,(route,mut base_segments,mut fs_segments))|{

           let mut segments = vec![];

           segments.append( & mut base_segments );

           if let Some(mut fs_segments) = fs_segments {
               segments.push(Segment::file_sys_root());
               segments.append(& mut fs_segments)
           }

           let route =match route {
               Some(route) => {
                   route
               }
               None => {
                   Route::default()
               }
           };

           (next, PointDef {
               route,
               segments
           })
       })
    }

}

fn point_segment<I>(input: I) -> Res<I,PointSeg> where I: Span {
    context("point-segment", alt((base_point_segment,version_point_segment)))(input)
}

fn base_point_segment<I>(input: I) -> Res<I,PointSeg> where I:Span {
    skewer_case(input).map(|(next,skewer)|{
        ( next, PointSeg::Base(skewer.to_string()))
    })
}

fn version_point_segment<I>(input: I) -> Res<I,PointSeg> where I:Span {
    version(input).map(|(next,version)|{
        ( next, PointSeg::Version(version))
    })
}

fn filesys_point_segment<I>(input: I) -> Res<I,PointSeg> where I:Span {
    context("point-segment-filesys" ,alt( (dir_point_segment,file_point_segment) ))(input)
}

fn dir_point_segment<I>(input: I) -> Res<I,PointSeg> where I:Span {
    pair( file_chars, peek(tag("/")))(input).map( |(next,(dir,_))| {
        (next,
         PointSeg::Dir(format!("{}/",dir.to_string()))
        )
    } )
}

fn file_point_segment<I>(input: I) -> Res<I,PointSeg> where I:Span {
     file_chars(input).map( |(next,file)|{
         (next, PointSeg::File(file.to_string()))
     })
}

pub fn point<I>(input: I) -> Res<I,Point> where I: Span {
    point_def( point_route_segment, point_segment, filesys_point_segment )(input)
}
