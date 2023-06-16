use crate::{
    common::{Attribute, FieldDefinition, Relation},
    condition::ConditionExpression,
};

pub struct SelectStatement {
    sel_list: Vec<Attribute>,
    from_list: Vec<Relation>,
    condition: Option<ConditionExpression>,
}

pub fn select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}

pub fn nested_select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    todo!()
}
