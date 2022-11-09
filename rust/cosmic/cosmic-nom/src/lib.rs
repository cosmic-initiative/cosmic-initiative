#![allow(warnings)]

use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::sync::Arc;

use nom::character::complete::multispace0;
use nom::combinator::recognize;
use nom::error::{ErrorKind, ParseError};
use nom::sequence::delimited;
use nom::{
    AsBytes, AsChar, Compare, CompareResult, FindSubstring, IResult, InputIter, InputLength,
    InputTake, InputTakeAtPosition, Needed, Offset, Slice,
};
use nom_locate::LocatedSpan;
use nom_supreme::error::ErrorTree;
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

pub trait Span:
    Clone
    + ToString
    + AsBytes
    + Slice<Range<usize>>
    + Slice<RangeTo<usize>>
    + Slice<RangeFrom<usize>>
    + InputLength
    + Offset
    + InputTake
    + InputIter<Item = char>
    + InputTakeAtPosition<Item = char>
    + Compare<&'static str>
    + FindSubstring<&'static str>
    + core::fmt::Debug
where
    Self: Sized,
    <Self as InputTakeAtPosition>::Item: AsChar,
{
    fn location_offset(&self) -> usize;

    fn location_line(&self) -> u32;

    fn get_column(&self) -> usize;

    fn extra(&self) -> Arc<String>;

    fn len(&self) -> usize;

    fn range(&self) -> Range<usize>;

    fn trace(&self) -> Trace {
        Trace {
            range: self.range(),
            extra: self.extra(),
        }
    }
}

impl<'a> Span for Wrap<LocatedSpan<&'a str, Arc<String>>> {
    fn location_offset(&self) -> usize {
        self.input.location_offset()
    }

    fn get_column(&self) -> usize {
        self.input.get_column()
    }

    fn location_line(&self) -> u32 {
        self.input.location_line()
    }

    fn extra(&self) -> Arc<String> {
        self.input.extra.clone()
    }

    fn len(&self) -> usize {
        self.input.len()
    }

    fn range(&self) -> Range<usize> {
        Range {
            start: self.location_offset(),
            end: self.location_offset() + self.len(),
        }
    }
}

// TraceWrap
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Tw<W> {
    pub trace: Trace,
    pub w: W,
}

impl<W> Tw<W> {
    pub fn new<I: Span>(span: I, w: W) -> Self {
        Self {
            trace: span.trace(),
            w,
        }
    }

    pub fn unwrap(self) -> W {
        self.w
    }

    pub fn replace<N>(self, n : N ) -> Tw<N> {
        Tw {
            trace: self.trace,
            w: n
        }
    }
}

impl<W> ToString for Tw<W>
where
    W: ToString,
{
    fn to_string(&self) -> String {
        self.w.to_string()
    }
}

impl<W> Deref for Tw<W> {
    type Target = W;

    fn deref(&self) -> &Self::Target {
        &self.w
    }
}

pub fn tw<I, F, O>(mut f: F) -> impl FnMut(I) -> Res<I, Tw<O>>
where
    I: Span,
    F: FnMut(I) -> Res<I, O>+Copy,
{
    move |input: I| {
        let (next, output) = f(input.clone())?;

        let span = input.slice(0..next.len());
        let tw = Tw::new(span, output);

        Ok((next, tw))
    }
}

//pub type OwnedSpan<'a> = LocatedSpan<&'a str, SpanExtra>;
pub type SpanExtra = Arc<String>;

pub fn new_span<'a>(s: &'a str) -> Wrap<LocatedSpan<&'a str, Arc<String>>> {
    let extra = Arc::new(s.to_string());
    let span = LocatedSpan::new_extra(s, extra);
    Wrap::new(span)
}

pub fn span_with_extra<'a>(
    s: &'a str,
    extra: Arc<String>,
) -> Wrap<LocatedSpan<&'a str, Arc<String>>> {
    Wrap::new(LocatedSpan::new_extra(s, extra))
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct Trace {
    pub range: Range<usize>,
    pub extra: SpanExtra,
}

impl Trace {
    pub fn new(range: Range<usize>, extra: SpanExtra) -> Self {
        Self { range, extra }
    }

    pub fn at_offset(offset: usize, extra: SpanExtra) -> Self {
        Self {
            range: offset..offset,
            extra,
        }
    }

    pub fn scan<F, I: Span, O>(f: F, input: I) -> Self
    where
        F: FnMut(I) -> Res<I, O> + Copy,
    {
        let extra = input.extra();
        let range = input.location_offset()..len(f)(input);
        Self { range, extra }
    }
}

#[derive(Debug, Clone)]
pub struct SliceStr {
    location_offset: usize,
    len: usize,
    string: Arc<String>,
}

impl ToString for SliceStr {
    fn to_string(&self) -> String {
        self.string
            .as_str()
            .slice(self.location_offset..self.location_offset + self.len)
            .to_string()
    }
}

impl SliceStr {
    pub fn new(string: String) -> Self {
        Self::from_arc(Arc::new(string))
    }

    pub fn from_arc(string: Arc<String>) -> Self {
        Self {
            len: string.len(),
            string,
            location_offset: 0,
        }
    }

    pub fn from(string: Arc<String>, location_offset: usize, len: usize) -> Self {
        Self {
            string,
            location_offset,
            len,
        }
    }
}

impl SliceStr {
    pub fn as_str(&self) -> &str {
        &self
            .string
            .as_str()
            .slice(self.location_offset..self.location_offset + self.len)
    }
}

impl Deref for SliceStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsBytes for SliceStr {
    fn as_bytes(&self) -> &[u8] {
        println!("AS BYTES: {}", self.string.as_bytes().len());
        self.string
            .as_bytes()
            .slice(self.location_offset..self.location_offset + self.len)
    }
}

impl Slice<Range<usize>> for SliceStr {
    fn slice(&self, range: Range<usize>) -> Self {
        SliceStr {
            location_offset: self.location_offset + range.start,
            len: range.end - range.start,
            string: self.string.clone(),
        }
    }
}

impl Slice<RangeFrom<usize>> for SliceStr {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        SliceStr {
            location_offset: self.location_offset + range.start,
            len: self.len - range.start,
            string: self.string.clone(),
        }
    }
}

impl Slice<RangeTo<usize>> for SliceStr {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        SliceStr {
            location_offset: self.location_offset,
            len: range.end,
            string: self.string.clone(),
        }
    }
}

impl Compare<&str> for SliceStr {
    fn compare(&self, t: &str) -> CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &str) -> CompareResult {
        self.as_str().compare_no_case(t)
    }
}

impl InputLength for SliceStr {
    fn input_len(&self) -> usize {
        self.len
    }
}

impl Offset for SliceStr {
    fn offset(&self, second: &Self) -> usize {
        self.location_offset
    }
}

pub struct MyCharIterator {}

pub struct MyChars {
    index: usize,
    slice: SliceStr,
}

impl MyChars {
    pub fn new(slice: SliceStr) -> Self {
        Self { index: 0, slice }
    }
}

impl Iterator for MyChars {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.slice.as_str().chars();
        let next = chars.nth(self.index);
        match next {
            None => None,
            Some(next) => {
                self.index = self.index + 1;
                Some(next)
            }
        }
    }
}

pub struct CharIterator {
    index: usize,
    slice: SliceStr,
}

impl CharIterator {
    pub fn new(slice: SliceStr) -> Self {
        Self { index: 0, slice }
    }
}

impl Iterator for CharIterator {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.slice.as_str().chars();
        let next = chars.nth(self.index);
        match next {
            None => None,
            Some(next) => {
                //let byte_index = self.index * std::mem::size_of::<char>();
                let byte_index = self.index;
                self.index = self.index + 1;
                Some((byte_index, next))
            }
        }
    }
}

impl InputIter for SliceStr {
    type Item = char;
    type Iter = CharIterator;
    type IterElem = MyChars;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        CharIterator::new(self.clone())
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        MyChars::new(self.clone())
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_str().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_str().slice_index(count)
    }
}

impl InputTakeAtPosition for SliceStr {
    type Item = char;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.as_str().position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.as_str().position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => {
                if self.as_str().input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

impl InputTake for SliceStr {
    fn take(&self, count: usize) -> Self {
        self.slice(count..)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (self.slice(count..), self.slice(..count))
    }
}

impl FindSubstring<&str> for SliceStr {
    fn find_substring(&self, substr: &str) -> Option<usize> {
        self.as_str().find_substring(substr)
    }
}

#[cfg(test)]
pub mod test {
    use nom::Slice;

    use crate::SliceStr;

    #[test]
    pub fn test() {
        let s = SliceStr::new("abc123".to_string());
        assert_eq!(6, s.len());

        let s = s.slice(0..3);
        assert_eq!(3, s.len());
        assert_eq!("abc", s.as_str());

        println!("bytes: {}", s.as_bytes().len());
        println!("chars: {}", s.chars().count());

        let s = SliceStr::new("abc123".to_string());
        assert_eq!("123", s.slice(3..).as_str());
        assert_eq!("abc", s.slice(..3).as_str());
    }
}

#[derive(Debug, Clone)]
pub struct Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    input: I,
}

impl<I> Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    pub fn new(input: I) -> Self {
        Self { input }
    }
}

impl<I> Deref for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

impl<I> AsBytes for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn as_bytes(&self) -> &[u8] {
        self.input.as_bytes()
    }
}

impl<I> Slice<Range<usize>> for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn slice(&self, range: Range<usize>) -> Self {
        Self::new(self.input.slice(range))
    }
}

impl<I> Slice<RangeFrom<usize>> for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self::new(self.input.slice(range))
    }
}

impl<I> Slice<RangeTo<usize>> for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self::new(self.input.slice(range))
    }
}

impl<'a> Compare<&'static str> for Wrap<LocatedSpan<&'a str, Arc<String>>> {
    fn compare(&self, t: &str) -> CompareResult {
        self.input.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> CompareResult {
        self.input.compare_no_case(t)
    }
}

impl<I> InputLength for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn input_len(&self) -> usize {
        self.input.input_len()
    }
}

impl<I> Offset for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn offset(&self, second: &Self) -> usize {
        self.input.offset(&second.input)
    }
}

impl<I> InputIter for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    type Item = <I as InputIter>::Item;
    type Iter = <I as InputIter>::Iter;
    type IterElem = <I as InputIter>::IterElem;

    fn iter_indices(&self) -> Self::Iter {
        self.input.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.input.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.input.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.input.slice_index(count)
    }
}

impl<I> InputTake for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn take(&self, count: usize) -> Self {
        Wrap::new(self.input.take(count))
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = self.input.take_split(count);
        (Wrap::new(left), Wrap::new(right))
    }
}

impl<I> ToString for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    fn to_string(&self) -> String {
        self.input.to_string()
    }
}

impl<'a> FindSubstring<&str> for Wrap<LocatedSpan<&'a str, Arc<String>>> {
    fn find_substring(&self, substr: &str) -> Option<usize> {
        self.input.find_substring(substr)
    }
}

impl<I> InputTakeAtPosition for Wrap<I>
where
    I: Clone
        + ToString
        + AsBytes
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>
        + InputLength
        + Offset
        + InputTake
        + InputIter<Item = char>
        + core::fmt::Debug
        + InputTakeAtPosition<Item = char>,
{
    type Item = <I as InputIter>::Item;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position1(predicate, e) {
            Err(nom::Err::Incomplete(_)) => {
                if self.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
            res => res,
        }
    }
}

pub type Res<I: Span, O> = IResult<I, O, ErrorTree<I>>;

pub fn wrap<I, F, O>(mut f: F) -> impl FnMut(I) -> Res<I, O>
where
    I: Span,
    F: FnMut(I) -> Res<I, O> + Copy,
{
    move |input: I| f(input)
}

pub fn len<I, F, O>(f: F) -> impl FnMut(I) -> usize
where
    I: Span,
    F: FnMut(I) -> Res<I, O> + Copy,
{
    move |input: I| match recognize(wrap(f))(input) {
        Ok((_, span)) => span.len(),
        Err(_) => 0,
    }
}

pub fn trim<I, F, O>(f: F) -> impl FnMut(I) -> Res<I, O>
where
    I: Span,
    F: FnMut(I) -> Res<I, O> + Copy,
{
    move |input: I| delimited(multispace0, f, multispace0)(input)
}
