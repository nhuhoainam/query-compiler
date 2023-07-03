use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    multi::many0,
    sequence::terminated,
};

use crate::{
    column::Column,
    common::{
        column_identifier, literal_expression, table_reference, ws_sep_comma,
        FieldDefinitionExpression, FieldValueExpression,
    },
    condition::ConditionExpression,
    join::{JoinConstraint, JoinOperator, JoinRightHand},
    table::Table, order::OrderByClause,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum FromItem {
    Table(Table),
    NestedSelect(SelectStatement),
    Join(JoinClause),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct JoinClause {
    pub operator: JoinOperator,
    pub right: JoinRightHand,
    pub condition: JoinConstraint,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct GroupByClause {
    pub columns: Vec<Column>,
    pub having: Option<ConditionExpression>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct SelectStatement {
    distinct: bool,
    sel_list: Vec<FieldDefinitionExpression>,
    tables: Vec<Table>,
    join: Vec<JoinClause>,
    condition: Option<ConditionExpression>,
    group_by: Option<GroupByClause>,
    order_by: Option<OrderByClause>,
}

impl fmt::Display for SelectStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

pub fn field_definition_expression(
    i: &[u8],
) -> nom::IResult<&[u8], Vec<FieldDefinitionExpression>> {
    many0(terminated(
        alt((
            map(tag("*"), |_| FieldDefinitionExpression::All),
            map(terminated(table_reference, tag(".*")), |table| {
                FieldDefinitionExpression::AllFromTable(table)
            }),
            map(column_identifier, |col| {
                FieldDefinitionExpression::Column(col)
            }),
            map(literal_expression, |lit| {
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(lit))
            }),
        )),
        opt(ws_sep_comma),
    ))(i)
}

pub fn select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}

pub fn nested_select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}
