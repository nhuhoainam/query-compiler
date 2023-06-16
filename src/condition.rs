use crate::{
    common::{Attribute, Operator},
    select::SelectStatement,
};

pub enum ConditionBase {
    Attr(Attribute),
    NestedSelect(Box<SelectStatement>),
}

pub struct ConditionTree {
    left: Box<ConditionExpression>,
    operator: Operator,
    right: Box<ConditionExpression>,
}

pub enum ConditionExpression {
    ComparisonOp(ConditionTree),
    LogicalOp(ConditionTree),
    NegationOp(Box<ConditionExpression>),
    ExistsOp(Box<SelectStatement>),
    Base(ConditionBase),
    Bracketed(Box<ConditionExpression>),
}
