use std::fmt::{self, Formatter};

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
pub struct Arithmetic {
    pub operator: ArithmeticOperator,
    pub left: ArithmeticOperand,
    pub right: ArithmeticOperand,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ArithmeticBase {
    Column(Column),
    Scalar(Literal),
    Bracketed(Box<Arithmetic>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ArithmeticOperand {
    Base(ArithmeticBase),
    Expression(Box<Arithmetic>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
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
            ArithmeticBase::Bracketed(ref ari) => write!(f, "({})", ari),
        }
    }
}

impl fmt::Display for ArithmeticOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ArithmeticOperand::Base(ref b) => write!(f, "{}", b),
            ArithmeticOperand::Expression(ref expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Arithmetic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

impl fmt::Display for ArithmeticExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.alias {
            Some(ref alias) => write!(f, "{} AS {}", self.ari, alias),
            None => write!(f, "{}", self.ari),
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
        | ArithmeticOperand::Base(ArithmeticBase::Scalar(_)) => Err(nom::Err::Error(
            nom::error::Error::new(i, nom::error::ErrorKind::Tag),
        )), // no operator
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
    map(pair(term, many0(expr_rest)), |(operand, rs)| {
        rs.into_iter().fold(operand, |acc, (op, r)| {
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

/// Parse simple arithmetic expressions combining literals, and columns and literals.
pub fn arithmetic_expression(i: &[u8]) -> IResult<&[u8], ArithmeticExpression> {
    map(pair(arithmetic, opt(as_alias)), |(ari, opt_alias)| {
        ArithmeticExpression {
            ari,
            alias: opt_alias.map(String::from),
        }
    })(i)
}

#[cfg(test)]
mod tests {
    use crate::{column::Column, common::Literal};

    use super::*;

    #[test]
    fn simple_arithmetic() {
        let inputs = ["1 + 1", "3 * 4", "foo - bar", "bar / 2"];
        let expected_outputs = [
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticOperator::Add,
                None,
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(3)),
                ArithmeticBase::Scalar(Literal::Integer(4)),
                ArithmeticOperator::Multiply,
                None,
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("foo"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticOperator::Subtract,
                None,
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Scalar(Literal::Integer(2)),
                ArithmeticOperator::Divide,
                None,
            ),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }

    #[test]
    fn arithmetic_with_alias() {
        let inputs = [
            "1 + 1 as foo",
            "3 * 4 as bar",
            "foo - bar as baz",
            "bar / 2 as qux",
        ];
        let expected_outputs = [
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticOperator::Add,
                Some(String::from("foo")),
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(3)),
                ArithmeticBase::Scalar(Literal::Integer(4)),
                ArithmeticOperator::Multiply,
                Some(String::from("bar")),
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("foo"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticOperator::Subtract,
                Some(String::from("baz")),
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Scalar(Literal::Integer(2)),
                ArithmeticOperator::Divide,
                Some(String::from("qux")),
            ),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }

    #[test]
    fn nested_arithmetic() {
        let inputs = [
            "1 + 1 + 1",
            "3 * (4 + 5)",
            "foo - bar + baz",
            "bar / 2 + 3",
            "1 + 1 * 2",
            "3 * 4 - 5",
            "foo - bar * baz",
            "bar / 2 / 3",
        ];
        let expected_outputs = [
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Add,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Multiply,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                    right: ArithmeticOperand::Base(ArithmeticBase::Bracketed(Box::new(
                        Arithmetic {
                            operator: ArithmeticOperator::Add,
                            left: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(4),
                            )),
                            right: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(5),
                            )),
                        },
                    ))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Subtract,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("foo"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("baz"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(4))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(5))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("foo"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("baz"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Divide,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: None,
            },
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }

    #[test]
    fn nested_arithmetic_with_alias() {
        let inputs = [
            "1 + 1 + 1 as foo",
            "3 * (4 + 5) as bar",
            "foo - bar + baz as baz",
            "bar / 2 + 3 as qux",
            "1 + 1 * 2 as foo",
            "3 * 4 - 5 as bar",
            "foo - bar * baz as baz",
            "bar / 2 / 3 as qux",
        ];

        let expected_outputs = [
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Add,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                },
                alias: Some(String::from("foo")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Multiply,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                    right: ArithmeticOperand::Base(ArithmeticBase::Bracketed(Box::new(
                        Arithmetic {
                            operator: ArithmeticOperator::Add,
                            left: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(4),
                            )),
                            right: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(5),
                            )),
                        },
                    ))),
                },
                alias: Some(String::from("bar")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Subtract,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("foo"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("baz"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                },
                alias: Some(String::from("baz")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: Some(String::from("qux")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                },
                alias: Some(String::from("foo")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(4))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(5))),
                },
                alias: Some(String::from("bar")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("foo"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("baz"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                },
                alias: Some(String::from("baz")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Divide,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: Some(String::from("qux")),
            },
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }
}
