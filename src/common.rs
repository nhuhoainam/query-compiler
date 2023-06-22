use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case, take, take_while1},
    character::{
        complete::{digit1, line_ending, multispace0, multispace1},
        is_alphanumeric,
    },
    combinator::{map, not, opt, peek},
    error::{ErrorKind, ParseError},
    multi::fold_many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, InputLength, Parser,
};

use crate::{arithmetic::ArithmeticExpression, keywords::sql_keywords};
use std::str::{self, FromStr};

pub struct Column {
    pub name: String,
    pub alias: Option<String>,
    pub table: Option<String>,
}

pub enum FieldDefinitionExpression {
    All,
    AllFromTable(String),
    Column(Column),
    FieldValue(FieldValueExpression),
}

pub enum FieldValueExpression {
    Arithmetic(ArithmeticExpression),
    Literal(LiteralExpression),
}

pub enum Operator {
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Like,
    NotLike,
    In,
    NotIn,
    Is,
}

pub struct Real {
    pub integral: i32,
    pub fractional: i32,
}

pub enum Literal {
    Null,
    String(String),
    Blob(Vec<u8>),
    Integer(i64),
    Decimal(f64),
    FixedPoint(Real),
    DateTime(String),
}

pub struct LiteralExpression {
    pub value: Literal,
    pub alias: Option<String>,
}

#[inline]
pub fn is_sql_identifier(chr: u8) -> bool {
    is_alphanumeric(chr) || chr == '_' as u8 || chr == '@' as u8
}

/// Parse a SQL identifier (alphanumeric1 and "_").
pub fn sql_identifier(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        preceded(not(peek(sql_keywords)), take_while1(is_sql_identifier)),
        delimited(tag("`"), take_while1(is_sql_identifier), tag("`")),
        delimited(tag("["), take_while1(is_sql_identifier), tag("]")),
    ))(i)
}

/// Parse optional parentheses () around expressions
pub(crate) fn opt_delimited<I: Clone, O1, O2, O3, E: ParseError<I>, F, G, H>(
    mut first: F,
    mut second: G,
    mut third: H,
) -> impl FnMut(I) -> IResult<I, O2, E>
where
    F: Parser<I, O1, E>,
    G: Parser<I, O2, E>,
    H: Parser<I, O3, E>,
{
    move |input: I| {
        let inp = input.clone();
        match second.parse(input) {
            Ok((i, o)) => Ok((i, o)),
            _ => {
                let (inp, _) = first.parse(inp)?;
                let (inp, o2) = second.parse(inp)?;
                third.parse(inp).map(|(i, _)| (i, o2))
            }
        }
    }
}

/// Parse rule for whitespace separated comma
pub(crate) fn ws_sep_comma(i: &[u8]) -> IResult<&[u8], &[u8]> {
    delimited(multispace0, tag(","), multispace0)(i)
}

/// Parse rule for whitespace separated equal sign
pub(crate) fn ws_sep_equals<'a, I>(i: I) -> IResult<I, I>
where
    I: nom::InputTakeAtPosition + nom::InputTake + nom::Compare<&'a str>,
    // Compare required by tag
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
    // AsChar and Clone required by multispace0
{
    delimited(multispace0, tag("="), multispace0)(i)
}

/// Parse rule for AS-based aliases for SQL entities.
pub fn as_alias(i: &[u8]) -> IResult<&[u8], &str> {
    map(
        tuple((
            multispace1,
            opt(pair(tag_no_case("as"), multispace1)),
            sql_identifier,
        )),
        |a| str::from_utf8(a.2).unwrap(),
    )(i)
}

/// Parse a SQL column identifier in the table.column format
pub fn column_identifier_no_alias(i: &[u8]) -> IResult<&[u8], Column> {
    let table_parser = pair(opt(terminated(sql_identifier, tag("."))), sql_identifier);

    map(table_parser, |tup| Column {
        name: str::from_utf8(tup.1).unwrap().to_string(),
        alias: None,
        table: match tup.0 {
            None => None,
            Some(t) => Some(str::from_utf8(t).unwrap().to_string()),
        },
    })(i)
}

/// Parse a SQL column identifier in the table.column format
pub fn column_identifier(i: &[u8]) -> IResult<&[u8], Column> {
    map(
        tuple((
            opt(terminated(sql_identifier, tag("."))),
            sql_identifier,
            opt(as_alias),
        )),
        |tup| Column {
            name: str::from_utf8(tup.1).unwrap().to_string(),
            alias: match tup.2 {
                None => None,
                Some(a) => Some(String::from(a)),
            },
            table: match tup.0 {
                None => None,
                Some(t) => Some(str::from_utf8(t).unwrap().to_string()),
            },
        },
    )(i)
}

pub(crate) fn eof<I: Copy + InputLength, E: ParseError<I>>(input: I) -> IResult<I, I, E> {
    if input.input_len() == 0 {
        Ok((input, input))
    } else {
        Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Eof)))
    }
}

/// Parse a terminator that ends a SQL statement.
pub fn statement_terminator(i: &[u8]) -> IResult<&[u8], ()> {
    let (remaining_input, _) =
        delimited(multispace0, alt((tag(";"), line_ending, eof)), multispace0)(i)?;

    Ok((remaining_input, ()))
}

/// Parse binary comparison operators
pub fn binary_comparison_operator(i: &[u8]) -> IResult<&[u8], Operator> {
    alt((
        map(tag_no_case("and"), |_| Operator::And),
        map(tag_no_case("or"), |_| Operator::Or),
        map(tag_no_case("="), |_| Operator::Equal),
        map(tag_no_case("!="), |_| Operator::NotEqual),
        map(tag_no_case("<>"), |_| Operator::NotEqual),
        map(tag_no_case("<"), |_| Operator::Less),
        map(tag_no_case("<="), |_| Operator::LessOrEqual),
        map(tag_no_case(">"), |_| Operator::Greater),
        map(tag_no_case(">="), |_| Operator::GreaterOrEqual),
        map(tag_no_case("like"), |_| Operator::Like),
        map(tag_no_case("not_like"), |_| Operator::NotLike),
        map(tag_no_case("in"), |_| Operator::In),
        map(tag_no_case("not_in"), |_| Operator::NotIn),
        map(tag_no_case("is"), |_| Operator::Is),
    ))(i)
}

fn unpack(v: &[u8]) -> i32 {
    i32::from_str(str::from_utf8(v).unwrap()).unwrap()
}

/// Parse floating point literal value
pub fn float_literal(i: &[u8]) -> IResult<&[u8], Literal> {
    map(tuple((opt(tag("-")), digit1, tag("."), digit1)), |tup| {
        Literal::FixedPoint(Real {
            integral: if (tup.0).is_some() {
                -1 * unpack(tup.1)
            } else {
                unpack(tup.1)
            },
            fractional: unpack(tup.3) as i32,
        })
    })(i)
}

/// Parse integer literal value
pub fn integer_literal(i: &[u8]) -> IResult<&[u8], Literal> {
    map(pair(opt(tag("-")), digit1), |tup| {
        let mut intval = i64::from_str(str::from_utf8(tup.1).unwrap()).unwrap();
        if (tup.0).is_some() {
            intval *= -1;
        }
        Literal::Integer(intval)
    })(i)
}

/// String literal value
fn raw_string_quoted(input: &[u8], is_single_quote: bool) -> IResult<&[u8], Vec<u8>> {
    let quote_slice: &[u8] = if is_single_quote { b"\'" } else { b"\"" };
    let double_quote_slice: &[u8] = if is_single_quote { b"\'\'" } else { b"\"\"" };
    let backslash_quote: &[u8] = if is_single_quote { b"\\\'" } else { b"\\\"" };
    delimited(
        tag(quote_slice),
        fold_many0(
            alt((
                is_not(backslash_quote),
                map(tag(double_quote_slice), |_| -> &[u8] {
                    if is_single_quote {
                        b"\'"
                    } else {
                        b"\""
                    }
                }),
                map(tag("\\\\"), |_| &b"\\"[..]),
                map(tag("\\b"), |_| &b"\x7f"[..]),
                map(tag("\\r"), |_| &b"\r"[..]),
                map(tag("\\n"), |_| &b"\n"[..]),
                map(tag("\\t"), |_| &b"\t"[..]),
                map(tag("\\0"), |_| &b"\0"[..]),
                map(tag("\\Z"), |_| &b"\x1A"[..]),
                preceded(tag("\\"), take(1usize)),
            )),
            || Vec::new(),
            |mut acc: Vec<u8>, bytes: &[u8]| {
                acc.extend(bytes);
                acc
            },
        ),
        tag(quote_slice),
    )(input)
}

fn raw_string_single_quoted(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    raw_string_quoted(i, true)
}

fn raw_string_double_quoted(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    raw_string_quoted(i, false)
}

pub fn string_literal(i: &[u8]) -> IResult<&[u8], Literal> {
    map(
        alt((raw_string_single_quoted, raw_string_double_quoted)),
        |bytes| match String::from_utf8(bytes) {
            Ok(s) => Literal::String(s),
            Err(err) => Literal::Blob(err.into_bytes()),
        },
    )(i)
}

// Parse any literal value.
pub fn literal(i: &[u8]) -> IResult<&[u8], Literal> {
    alt((
        float_literal,
        integer_literal,
        string_literal,
        map(tag_no_case("null"), |_| Literal::Null),
    ))(i)
}

pub fn literal_expression(i: &[u8]) -> IResult<&[u8], LiteralExpression> {
    map(
        pair(opt_delimited(tag("("), literal, tag(")")), opt(as_alias)),
        |p| LiteralExpression {
            value: p.0,
            alias: (p.1).map(|a| a.to_string()),
        },
    )(i)
}
