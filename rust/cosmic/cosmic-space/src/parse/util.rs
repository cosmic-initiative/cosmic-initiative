use cosmic_nom::{Res, Span};
use nom::branch::alt;
use nom::combinator::{eof, value};
use nom::bytes::complete::tag;
use nom::sequence::preceded;
use crate::kind::{OptPattern, Pattern};

pub fn pattern<I, FnX, X>(mut f: FnX) -> impl FnMut(I) -> Res<I, Pattern<X>> + Copy
where
    I: Span,
    FnX: FnMut(I) -> Res<I, X> + Copy,
    X: Clone,
{
    move |input| {
        alt((
            value(Pattern::Any, tag("*")),
            value(Pattern::None, tag("!")),
            |i| f(i).map(|(next, x)| (next, Pattern::Matches(x))),
        ))(input)
    }
}

pub fn opt_pattern<I, FnX, X>(mut f: FnX) -> impl FnMut(I) -> Res<I, OptPattern<X>> + Copy
where
    I: Span,
    FnX: FnMut(I) -> Res<I, X> + Copy,
    X: Clone,
{
    move |input| {
        alt((
            value(OptPattern::Any, tag("*")),
            value(OptPattern::None, tag("!")),
            value(OptPattern::None, eof),
            |i| f(i).map(|(next, x)| (next, OptPattern::Matches(x))),
        ))(input)
    }
}

pub fn preceded_opt_pattern<I, FnX, X, FnPrec>(
    prec: FnPrec,
    mut f: FnX,
) -> impl FnMut(I) -> Res<I, OptPattern<X>> + Copy
where
    I: Span,
    FnX: FnMut(I) -> Res<I, X> + Copy,
    X: Clone,
    FnPrec: FnMut(I) -> Res<I, I> + Copy,
{
    move |input| {
        alt((preceded(prec, opt_pattern(f)), |i| {
            Ok((i, OptPattern::None))
        }))(input)
    }
}
