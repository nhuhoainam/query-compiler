use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{multispace0, multispace1},
    combinator::{map, opt},
    sequence::{delimited, tuple},
    IResult,
};

use crate::{
    arithmetic::ArithmeticExpression,
    column::Column,
    common::{binary_comparison_operator, Literal, Operator},
    select::{nested_select_statement, SelectStatement},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ConditionBase {
    Field(Column),
    Literal(Literal),
    LiteralList(Vec<Literal>),
    NestedSelect(Box<SelectStatement>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct ConditionTree {
    operator: Operator,
    left: Box<ConditionExpression>,
    right: Box<ConditionExpression>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ConditionExpression {
    ComparisonOp(ConditionTree),
    LogicalOp(ConditionTree),
    NegationOp(Box<ConditionExpression>),
    ExistsOp(Box<SelectStatement>),
    Base(ConditionBase),
    Arithmetic(Box<ArithmeticExpression>),
    Bracketed(Box<ConditionExpression>),
}

impl fmt::Display for ConditionBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConditionBase::Field(c) => write!(f, "{}", c),
            ConditionBase::Literal(l) => write!(f, "{}", l),
            ConditionBase::LiteralList(l) => {
                write!(
                    f,
                    "({})",
                    l.iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            ConditionBase::NestedSelect(s) => write!(f, "({})", s),
        }
    }
}

impl fmt::Display for ConditionTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

impl fmt::Display for ConditionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConditionExpression::ComparisonOp(c) => write!(f, "{}", c),
            ConditionExpression::LogicalOp(c) => write!(f, "{}", c),
            ConditionExpression::NegationOp(c) => write!(f, "NOT {}", c),
            ConditionExpression::ExistsOp(s) => write!(f, "EXISTS {}", s),
            ConditionExpression::Base(b) => write!(f, "{}", b),
            ConditionExpression::Arithmetic(a) => write!(f, "{}", a),
            ConditionExpression::Bracketed(c) => write!(f, "({})", c),
        }
    }
}

fn predicate(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let nested_exist = map(
        tuple((
            opt(delimited(multispace0, tag_no_case("not"), multispace1)),
            delimited(multispace0, tag_no_case("exist"), multispace0),
            delimited(multispace0, nested_select_statement, multispace0),
        )),
        |p| {
            let nested = ConditionExpression::ExistsOp(Box::new(p.2));
            match p.0 {
                Some(_) => ConditionExpression::NegationOp(Box::new(nested)),
                None => nested,
            }
        },
    );

    alt((simple_expr, nested_exist))(i)
}

fn simple_expr(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    todo!()
}

fn parenthesized_expr_helper(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let (remaining_input, (_, _, left_expr, _, _, _, operator, _, right_expr)) = tuple((
        tag("("),
        multispace0,
        simple_expr,
        multispace0,
        tag(")"),
        multispace0,
        binary_comparison_operator,
        multispace0,
        simple_expr,
    ))(i)?;

    let left = Box::new(ConditionExpression::Bracketed(Box::new(left_expr)));
    let right = Box::new(right_expr);
    let condition = ConditionExpression::ComparisonOp(ConditionTree {
        operator,
        left,
        right,
    });

    Ok((remaining_input, condition))
}
