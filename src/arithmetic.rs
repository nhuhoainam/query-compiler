use std::fmt::{self, Formatter};

use display_tree::{AsTree, DisplayTree, StyleBuilder};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};

use crate::{
    column::Column,
    common::{as_alias, column_identifier_no_alias, integer_literal, Literal},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Arithmetic {
    Base(ArithmeticBase),
    Expr {
        operator: ArithmeticOperator,
        left: Box<Self>,
        right: Box<Self>,
    },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ArithmeticBase {
    Column(Column),
    Scalar(Literal),
    Bracketed(Box<Arithmetic>),
}

// #[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize, DisplayTree)]
// pub enum ArithmeticOperand {
//     Base(ArithmeticBase),
//     Expression(Box<Arithmetic>),
// }

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize, DisplayTree)]
pub enum ArithmeticExpression {
    WithAlias {
        ari: Arithmetic,
        #[node_label]
        alias: String,
    },
    WithoutAlias(Arithmetic),
}

impl ArithmeticExpression {
    pub fn new(
        left: ArithmeticBase,
        right: ArithmeticBase,
        operator: ArithmeticOperator,
        alias: Option<String>,
    ) -> Self {
        match alias {
            Some(a) => ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator,
                    left: Box::new(Arithmetic::Base(left)),
                    right: Box::new(Arithmetic::Base(right)),
                },
                alias: a,
            },
            None => ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator,
                left: Box::new(Arithmetic::Base(left)),
                right: Box::new(Arithmetic::Base(right)),
            }),
        }
    }
}

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ArithmeticOperator::Add => write!(f, "+"),
            ArithmeticOperator::Subtract => write!(f, "-"),
            ArithmeticOperator::Multiply => write!(f, "*"),
            ArithmeticOperator::Divide => write!(f, "/"),
        }
    }
}

impl fmt::Display for ArithmeticBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ArithmeticBase::Column(ref col) => write!(f, "{}", col),
            ArithmeticBase::Scalar(ref lit) => write!(f, "{}", lit.to_string()),
            ArithmeticBase::Bracketed(ref ari) => {
                // write!(f, "{}", AsTree::new(&**ari).indentation(1))
                write!(f, "({})", ari)
            }
        }
    }
}

// impl fmt::Display for ArithmeticOperand {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match *self {
//             ArithmeticOperand::Base(ref b) => write!(f, "{}", b),
//             ArithmeticOperand::Expression(ref expr) => write!(f, "{}", expr),
//         }
//     }
// }

impl fmt::Display for Arithmetic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Arithmetic::Base(ref b) => write!(f, "{}", b),
            Arithmetic::Expr {
                ref operator,
                ref left,
                ref right,
            } => write!(f, "{} {} {}", left, operator, right),
        }
    }
}

// impl DisplayTree for Arithmetic {
//     fn fmt(&self, f: &mut Formatter<'_>, style: Style) -> fmt::Result {
//         match self {
//             Arithmetic::Base(base) => write!(f, "{}", base),
//             Arithmetic::Expr {
//                 ref operator,
//                 ref left,
//                 ref right,
//             } => {
//                 writeln!(f, "{}", operator)?;
//                 writeln!(
//                     f,
//                     "{}{} {}",
//                     style.char_set.connector,
//                     std::iter::repeat(style.char_set.horizontal)
//                         .take(style.indentation as usize)
//                         .collect::<String>(),
//                     left
//                 )?;
//                 write!(
//                     f,
//                     "{}{} {}",
//                     style.char_set.end_connector,
//                     std::iter::repeat(style.char_set.horizontal)
//                         .take(style.indentation as usize)
//                         .collect::<String>(),
//                     right
//                 )
//             }
//         }
//     }
// }

impl fmt::Display for ArithmeticExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArithmeticExpression::WithAlias { ari, alias } => {
                write!(f, "{} AS {}", ari, alias)
            }
            ArithmeticExpression::WithoutAlias(ari) => write!(f, "{}", ari),
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
        Arithmetic::Base(ArithmeticBase::Column(_))
        | Arithmetic::Base(ArithmeticBase::Scalar(_)) => Err(nom::Err::Error(
            nom::error::Error::new(i, nom::error::ErrorKind::Tag),
        )), // no operator
        Arithmetic::Base(ArithmeticBase::Bracketed(expr)) => Ok((res.0, *expr)),
        Arithmetic::Expr {
            operator,
            left,
            right,
        } => Ok((
            res.0,
            Arithmetic::Expr {
                operator,
                left,
                right,
            },
        )),
    }
}

/// Parse a term
/// term is an arithmetic expression with multiplication or division
fn term(i: &[u8]) -> IResult<&[u8], Arithmetic> {
    map(pair(arithmetic_base, many0(term_rest)), |(b, rs)| {
        rs.into_iter()
            .fold(Arithmetic::Base(b), |acc, (op, r)| Arithmetic::Expr {
                operator: op,
                left: Box::new(acc),
                right: Box::new(r),
            })
    })(i)
}

/// Parse right hand side of the term
fn term_rest(i: &[u8]) -> IResult<&[u8], (ArithmeticOperator, Arithmetic)> {
    separated_pair(
        preceded(multispace0, mul_div_operator),
        multispace0,
        map(arithmetic_base, |b| Arithmetic::Base(b)),
    )(i)
}

/// Parse an expression
/// expression is an arithmetic expression with addition or subtraction
fn expr(i: &[u8]) -> IResult<&[u8], Arithmetic> {
    map(pair(term, many0(expr_rest)), |(operand, rs)| {
        rs.into_iter()
            .fold(operand, |acc, (op, r)| Arithmetic::Expr {
                operator: op,
                left: Box::new(acc),
                right: Box::new(r),
            })
    })(i)
}

/// Parse right hand side of the Expression
fn expr_rest(i: &[u8]) -> IResult<&[u8], (ArithmeticOperator, Arithmetic)> {
    separated_pair(preceded(multispace0, add_sub_operator), multispace0, term)(i)
}

/// Parse simple arithmetic expressions combining literals, and columns and literals.
pub fn arithmetic_expression(i: &[u8]) -> IResult<&[u8], ArithmeticExpression> {
    map(
        pair(arithmetic, opt(as_alias)),
        |(ari, opt_alias)| match opt_alias {
            Some(alias) => ArithmeticExpression::WithAlias {
                ari,
                alias: alias.to_string(),
            },
            None => ArithmeticExpression::WithoutAlias(ari),
        },
    )(i)
}
