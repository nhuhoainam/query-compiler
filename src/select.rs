use crate::{
    common::{FieldDefinitionExpression},
    condition::ConditionExpression,
};

pub enum FromItem {
    Table(String),
    NestedSelect(SelectStatement),
    Join(JoinItem),
}

pub enum JoinOperator {
    Inner,
    Left,
    Right,
    Full,
}

pub struct JoinItem {
    pub right: String,
    pub condition: Option<ConditionExpression>,
}

pub struct SelectStatement {
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
