use nom::{
    branch::alt,
    bytes::{complete::tag_no_case, streaming::tag},
    combinator::peek,
    error::{ErrorKind, ParseError},
    sequence::terminated,
    IResult, InputLength,
};

pub(crate) fn eof<I: Copy + InputLength, E: ParseError<I>>(input: I) -> IResult<I, I, E> {
    if input.input_len() == 0 {
        Ok((input, input))
    } else {
        Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Eof)))
    }
}

fn char_followed_by_keyword(i: &[u8]) -> IResult<&[u8], &[u8]> {
    peek(alt((
        tag(" "),
        tag("\n"),
        tag("\r"),
        tag("\t"),
        tag("("),
        tag(")"),
        tag(","),
        tag(";"),
        tag("="),
        eof,
    )))(i)
}

// each alt only allows tuples of up to 21 elements, so we have to split the keywords into many sets

fn keyword_set_1(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        terminated(tag_no_case("SELECT"), char_followed_by_keyword),
        terminated(tag_no_case("FROM"), char_followed_by_keyword),
        terminated(tag_no_case("WHERE"), char_followed_by_keyword),
        terminated(tag_no_case("GROUP"), char_followed_by_keyword),
        terminated(tag_no_case("HAVING"), char_followed_by_keyword),
        terminated(tag_no_case("ORDER"), char_followed_by_keyword),
        terminated(tag_no_case("BY"), char_followed_by_keyword),
        terminated(tag_no_case("DISTINCT"), char_followed_by_keyword),
        terminated(tag_no_case("OR"), char_followed_by_keyword),
        terminated(tag_no_case("NOT"), char_followed_by_keyword),
        terminated(tag_no_case("ORDER"), char_followed_by_keyword),
        terminated(tag_no_case("LIMIT"), char_followed_by_keyword),
        terminated(tag_no_case("OFFSET"), char_followed_by_keyword),
        terminated(tag_no_case("EXISTS"), char_followed_by_keyword),
        terminated(tag_no_case("JOIN"), char_followed_by_keyword),
        terminated(tag_no_case("NATURAL"), char_followed_by_keyword),
        terminated(tag_no_case("LEFT"), char_followed_by_keyword),
        terminated(tag_no_case("RIGHT"), char_followed_by_keyword),
        terminated(tag_no_case("INNER"), char_followed_by_keyword),
        terminated(tag_no_case("OUTER"), char_followed_by_keyword),
        terminated(tag_no_case("CROSS"), char_followed_by_keyword),
    ))(i)
}

fn keyword_set_2(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        terminated(tag_no_case("IS"), char_followed_by_keyword),
        terminated(tag_no_case("IN"), char_followed_by_keyword),
        terminated(tag_no_case("LIKE"), char_followed_by_keyword),
        terminated(tag_no_case("AND"), char_followed_by_keyword),
        terminated(tag_no_case("OR"), char_followed_by_keyword),
    ))(i)
}

// Matches the above SQL keywords, case-insensitive.
pub fn sql_keywords(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((keyword_set_1, keyword_set_2))(i)
}

pub fn escape_if_keyword(s: &str) -> String {
    if sql_keywords(s.as_bytes()).is_ok() {
        format!("`{}`", s)
    } else {
        s.to_string()
    }
}
