use crate::{
    arithmetic::ArithmeticExpression,
    column::Column,
    common::{Literal, Operator},
    select::SelectStatement,
};

pub enum ConditionBase {
    Field(Column),
    Literal(Literal),
    LiteralList(Vec<Literal>),
    NestedSelect(Box<SelectStatement>),
}

pub struct ConditionTree {
    operator: Operator,
    left: Box<ConditionExpression>,
    right: Box<ConditionExpression>,
}

pub enum ConditionExpression {
    ComparisonOp(ConditionTree),
    LogicalOp(ConditionTree),
    NegationOp(Box<ConditionExpression>),
    ExistsOp(Box<SelectStatement>),
    Base(ConditionBase),
    Arithmetic(Box<ArithmeticExpression>),
    Bracketed(Box<ConditionExpression>),
}
