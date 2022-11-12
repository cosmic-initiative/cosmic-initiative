use cosmic_nom::{Res, Span, tw};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::bytes::complete::tag;
use nom::combinator::{eof, opt, value};
use nom::branch::alt;
use crate::kind::{CamelCaseSubTypes, CamelCaseSubTypesSelector, KindDef, OptPattern, ParentChildDef, Pattern, ProtoKind, ProtoKindSelector, ProtoVariant, ProtoVariantSelector, Specific, SpecificDef, SpecificFullSelector, SpecificSelector, SpecificSubTypes, SpecificSubTypesSelector, SubTypeDef, VariantDef};
use crate::parse::{camel_case, domain, skewer_case, version, version_req};

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

fn sub_types<I, FnPart, Part, FnCamel, Camel>(
    fn_part: FnPart,
    fn_camel: FnCamel,
) -> impl FnMut(I) -> Res<I, SubTypeDef<Part, Camel>>
where
    FnPart: FnMut(I) -> Res<I, Part> + Copy,
    FnCamel: FnMut(I) -> Res<I, Camel> + Copy,
    I: Span,
{
    move |input: I| {
        tuple((fn_part, fn_camel, fn_camel))(input)
            .map(|(next, (part, sub, r#type))| (next, SubTypeDef { part, sub, r#type }))
    }
}

fn parent_child_def<I, FnParent, Parent, FnChild, Child>(
    fn_parent: FnParent,
    fn_child: FnChild,
) -> impl FnMut(I) -> Res<I, ParentChildDef<Parent, Child>>
where
    FnParent: FnMut(I) -> Res<I, Parent> + Copy,
    FnChild: FnMut(I) -> Res<I, Child> + Copy,
    I: Span,
{
    move |input: I| {
        pair(fn_parent, fn_child)(input)
            .map(|(next, (parent, child))| (next, ParentChildDef { parent, child }))
    }
}

pub fn specific_def<I, FnDomain, FnSkewer, FnVersion, Domain, Skewer, Version>(
    fn_domain: FnDomain,
    fn_skewer: FnSkewer,
    fn_version: FnVersion,
) -> impl FnMut(I) -> Res<I, SpecificDef<Domain, Skewer, Version>> + Copy
where
    I: Span,
    FnDomain: FnMut(I) -> Res<I, Domain> + Copy,
    FnSkewer: FnMut(I) -> Res<I, Skewer> + Copy,
    FnVersion: FnMut(I) -> Res<I, Version> + Copy,
{
    move |input: I| {
        tuple((
            fn_domain,
            tag(":"),
            fn_domain,
            tag(":"),
            fn_skewer,
            tag(":"),
            fn_skewer,
            tag(":"),
            fn_version,
        ))(input)
        .map(
            |(next, (provider, _, vendor, _, product, _, variant, _, version))| {
                (
                    next,
                    SpecificDef {
                        provider,
                        vendor,
                        product,
                        variant,
                        version,
                    },
                )
            },
        )
    }
}

pub fn specific<I>(input: I) -> Res<I, Specific>
where
    I: Span,
{
    specific_def(domain, skewer_case, version)(input)
}

pub fn specific_sub_types<I>(input: I) -> Res<I, SpecificSubTypes>
where
    I: Span,
{
    sub_types(specific, |i| opt(preceded(tag(":"), camel_case))(i))(input)
}

pub fn specific_selector<I>(input: I) -> Res<I, SpecificSelector>
where
    I: Span,
{
    specific_def(
        pattern(domain),
        pattern(skewer_case),
        pattern(|i| delimited(tag("("), version_req, tag(")"))(i)),
    )(input)
}

pub fn variant_selector<I>(input: I) -> Res<I, ProtoVariantSelector>
where
    I: Span,
{
    variant_def(
        |i| tw(pattern(camel_case_sub_types_selector))(i),
        |i| tw(|i| opt(child(pattern(specific_selector)))(i))(i),
    )(input)
    .map(|(next, variant)| {
        let child = match &variant.child.w {
            None => OptPattern::None,
            Some(p) => match p {
                Pattern::None => OptPattern::None,
                Pattern::Any => OptPattern::Any,
                Pattern::Matches(c) => OptPattern::Matches(c.clone()),
            },
        };

        (
            next,
            ProtoVariantSelector {
                parent: variant.parent,
                child: variant.child.replace(child),
            },
        )
    })
}

pub fn kind_selector<I>(input: I) -> Res<I, ProtoKindSelector>
where
    I: Span,
{
    variant_def(
        |i| tw(pattern(camel_case_sub_types_selector))(i),
        |i| tw(|i| opt(child(pattern(variant_selector)))(i))(i),
    )(input)
    .map(|(next, kind)| {
        let child = match &kind.child.w {
            None => OptPattern::None,
            Some(p) => match p {
                Pattern::None => OptPattern::None,
                Pattern::Any => OptPattern::Any,
                Pattern::Matches(c) => OptPattern::Matches(c.clone()),
            },
        };

        (
            next,
            ProtoKindSelector {
                parent: kind.parent,
                child: kind.child.replace(child),
            },
        )
    })
}

pub fn specific_sub_types_selector<I>(input: I) -> Res<I, SpecificSubTypesSelector>
where
    I: Span,
{
    sub_types(
        pattern(specific_selector),
        preceded_opt_pattern(|i| tag(":")(i), camel_case),
    )(input)
}

pub fn specific_full_selector<I>(input: I) -> Res<I, SpecificFullSelector>
where
    I: Span,
{
    sub_types(
        specific_selector,
        preceded_opt_pattern(|i| tag(":")(i), camel_case), //                  preceded_opt_pattern(|i|tag(":")(i), camel_case),
    )(input)
}

pub fn variant_def<I, FnVariant, Variant, FnSpecific, Specific>(
    variant: FnVariant,
    specific: FnSpecific,
) -> impl FnMut(I) -> Res<I, VariantDef<Variant, Specific>>
where
    I: Span,
    FnVariant: FnMut(I) -> Res<I, Variant> + Copy,
    FnSpecific: FnMut(I) -> Res<I, Specific> + Copy,
{
    move |input: I| parent_child_def(variant, specific)(input)
}

pub fn kind_def<I, FnKind, Kind, FnVariant, Variant>(
    fn_kind: FnKind,
    fn_variant: FnVariant,
) -> impl FnMut(I) -> Res<I, KindDef<Kind, Variant>>
where
    I: Span,
    FnKind: FnMut(I) -> Res<I, Kind> + Copy,
    FnVariant: FnMut(I) -> Res<I, Variant> + Copy,
{
    move |input: I| parent_child_def(fn_kind, fn_variant)(input)
}

pub fn camel_case_sub_types<I>(input: I) -> Res<I, CamelCaseSubTypes>
where
    I: Span,
{
    sub_types(camel_case, |i| opt(preceded(tag(":"), camel_case))(i))(input)
}

pub fn camel_case_sub_types_selector<I>(input: I) -> Res<I, CamelCaseSubTypesSelector>
where
    I: Span,
{
    sub_types(
        pattern(camel_case),
        preceded_opt_pattern(|i| tag(":")(i), camel_case),
    )(input)
}

pub fn child<I, F, R>(mut f: F) -> impl FnMut(I) -> Res<I, R>
where
    I: Span,
    F: FnMut(I) -> Res<I, R> + Copy,
{
    move |input: I| delimited(tag("<"), f, tag(">"))(input)
}

pub fn proto_variant<I>(input: I) -> Res<I, ProtoVariant>
where
    I: Span,
{
    variant_def(
        |i| tw(camel_case_sub_types)(i),
        |i| tw(|i| opt(child(specific_sub_types))(i))(i),
    )(input)
}

pub fn proto_kind<I>(input: I) -> Res<I, ProtoKind>
where
    I: Span,
{
    kind_def(
        |i| tw(camel_case_sub_types)(i),
        |i| tw(|i| opt(child(proto_variant))(i))(i),
    )(input)
}
