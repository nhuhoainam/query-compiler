use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::map,
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};

use crate::common::{
    column_identifier_no_alias, integer_literal, opt_delimited, sql_identifier, Column, Literal,
};

pub struct Arithmetic {
    pub operator: ArithmeticOperator,
    pub left: ArithmeticOperand,
    pub right: ArithmeticOperand,
}

pub enum ArithmeticBase {
    Column(Column),
    Scalar(Literal),
    Bracketed(Box<Arithmetic>),
}

pub enum ArithmeticOperand {
    Base(ArithmeticBase),
    Expression(Box<Arithmetic>),
}

pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub struct ArithmeticExpression {
    pub ari: Arithmetic,
    pub alias: Option<String>,
}

impl Arithmetic {
    pub fn new(operator: ArithmeticOperator, left: ArithmeticBase, right: ArithmeticBase) -> Self {
        Self {
            operator,
            left: ArithmeticOperand::Base(left),
            right: ArithmeticOperand::Base(right),
        }
    }
}

impl ArithmeticExpression {
    pub fn new(
        left: ArithmeticBase,
        right: ArithmeticBase,
        operator: ArithmeticOperator,
        alias: Option<String>,
    ) -> Self {
        Self {
            ari: Arithmetic {
                operator,
                left: ArithmeticOperand::Base(left),
                right: ArithmeticOperand::Base(right),
            },
            alias,
        }
    }
}

fn add_sub_operator(i: &[u8]) -> IResult<&[u8], ArithmeticOperator> {
    alt((
        map(tag("+"), |_| ArithmeticOperator::Add),
        map(tag("-"), |_| ArithmeticOperator::Subtract),
    ))(i)
}

fn mul_div_operator(i: &[u8]) -> IResult<&[u8], ArithmeticOperator> {
    alt((
        map(tag("*"), |_| ArithmeticOperator::Multiply),
        map(tag("/"), |_| ArithmeticOperator::Divide),
    ))(i)
}

fn arithmetic_base(i: &[u8]) -> IResult<&[u8], ArithmeticBase> {
    alt((
        map(integer_literal, |lit| ArithmeticBase::Scalar(lit)),
        map(column_identifier_no_alias, |col| {
            ArithmeticBase::Column(col)
        }),
        map(
            delimited(
                terminated(tag("("), multispace0),
                arithmetic,
                preceded(multispace0, tag(")")),
            ),
            |ari| ArithmeticBase::Bracketed(Box::new(ari)),
        ),
    ))(i)
}

fn arithmetic(i: &[u8]) -> IResult<&[u8], Arithmetic> {
    let res = expr(i)?;

    match res.1 {
        ArithmeticOperand::Base(ArithmeticBase::Column(_))
        | ArithmeticOperand::Base(ArithmeticBase::Scalar(_)) => {
            Err(nom::Err::Error(nom::error::Error::new(i, nom::error::ErrorKind::Tag)))
        } // no operator
        ArithmeticOperand::Base(ArithmeticBase::Bracketed(expr)) => Ok((res.0, *expr)),
        ArithmeticOperand::Expression(expr) => Ok((res.0, *expr)),
    }
}

/// Parse a term
/// term is an arithmetic expression with multiplication or division
fn term(i: &[u8]) -> IResult<&[u8], ArithmeticOperand> {
    map(pair(arithmetic_base, many0(term_rest)), |(b, rs)| {
        rs.into_iter()
            .fold(ArithmeticOperand::Base(b), |acc, (op, r)| {
                ArithmeticOperand::Expression(Box::new(Arithmetic {
                    operator: op,
                    left: acc,
                    right: r,
                }))
            })
    })(i)
}

/// Parse right hand side of the term
fn term_rest(i: &[u8]) -> IResult<&[u8], (ArithmeticOperator, ArithmeticOperand)> {
    separated_pair(
        preceded(multispace0, mul_div_operator),
        multispace0,
        map(arithmetic_base, |b| ArithmeticOperand::Base(b)),
    )(i)
}

/// Parse an expression
/// expression is an arithmetic expression with addition or subtraction
fn expr(i: &[u8]) -> IResult<&[u8], ArithmeticOperand> {
    map(pair(arithmetic_base, many0(expr_rest)), |(b, rs)| {
        rs.into_iter()
            .fold(ArithmeticOperand::Base(b), |acc, (op, r)| {
                ArithmeticOperand::Expression(Box::new(Arithmetic {
                    operator: op,
                    left: acc,
                    right: r,
                }))
            })
    })(i)
}

/// Parse right hand side of the Expression
fn expr_rest(i: &[u8]) -> IResult<&[u8], (ArithmeticOperator, ArithmeticOperand)> {
    separated_pair(preceded(multispace0, add_sub_operator), multispace0, term)(i)
}
