use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    combinator::{map, opt},
    multi::many0,
    sequence::{terminated, tuple}, IResult, character::complete::{multispace0, multispace1},
};

use crate::{
    column::Column,
    common::{
        column_identifier, literal_expression, table_reference, ws_sep_comma,
        FieldDefinitionExpression, FieldValueExpression, field_list,
    },
    condition::{ConditionExpression, condition_expr},
    join::JoinClause,
    table::Table, order::OrderByClause,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum FromItem {
    Table(Table),
    NestedSelect(SelectStatement),
    Join(JoinClause),
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

fn having_clause(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let (remaining_input, (_, _, _, ce)) = tuple((
        multispace0,
        tag_no_case("having"),
        multispace1,
        condition_expr,
    ))(i)?;

    Ok((remaining_input, ce))
}

/// Parse GROUP BY clause
pub fn group_by_clause(i: &[u8]) -> IResult<&[u8], GroupByClause> {
    let (remaining_input, (_, _, _, columns, having)) = tuple((
        multispace0,
        tag_no_case("group by"),
        multispace1,
        field_list,
        opt(having_clause),
    ))(i)?;

    Ok((remaining_input, GroupByClause { columns, having }))
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

/// Parse WHERE clause of a selection
pub fn where_clause(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let (remaining_input, (_, _, _, where_condition)) = tuple((
        multispace0,
        tag_no_case("where"),
        multispace1,
        condition_expr,
    ))(i)?;

    Ok((remaining_input, where_condition))
}

pub fn select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}

pub fn nested_select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}
