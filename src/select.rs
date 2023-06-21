use crate::{
    common::FieldDefinitionExpression,
    condition::ConditionExpression, join::{JoinOperator, JoinOperand}, table::Table,
};

pub enum FromItem {
    Table(Table),
    NestedSelect(SelectStatement),
    Join(JoinClause),
}

pub struct JoinClause {
    pub operator: JoinOperator,
    pub left: JoinOperand,
    pub right: JoinOperand,
    pub condition: Option<ConditionExpression>,
}

pub struct SelectStatement {
    distinct: bool,
    sel_list: Vec<FieldDefinitionExpression>,
    from_list: Vec<FromItem>,
    condition: Option<ConditionExpression>,
}

pub fn select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}

pub fn nested_select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}
