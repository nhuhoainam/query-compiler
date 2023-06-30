use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    multi::many0,
    sequence::terminated,
};

use crate::{
    common::{
        column_identifier, literal_expression, table_reference, ws_sep_comma,
        FieldDefinitionExpression, FieldValueExpression,
    },
    condition::ConditionExpression,
    join::{JoinOperator, JoinRightHand, JoinConstraint},
    table::Table,
};

pub enum FromItem {
    Table(Table),
    NestedSelect(SelectStatement),
    Join(JoinClause),
}

pub struct JoinClause {
    pub operator: JoinOperator,
    pub right: JoinRightHand,
    pub condition: JoinConstraint,
}

pub struct SelectStatement {
    distinct: bool,
    sel_list: Vec<FieldDefinitionExpression>,
    tables: Vec<Table>,
    join: Vec<JoinClause>,
    condition: Option<ConditionExpression>,
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
