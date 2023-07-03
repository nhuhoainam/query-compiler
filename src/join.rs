use crate::{
    column::Column,
    condition::ConditionExpression,
    select::{JoinClause, SelectStatement},
    table::Table,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum JoinRightHand {
    Table(Table),
    // A comma-separated (and implicitly joined) sequence of tables.
    Tables(Vec<Table>),
    // A nested selection, represented as (query, alias).
    NestedSelect(Box<SelectStatement>, Option<String>),
    NestedJoin(Box<JoinClause>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum JoinOperator {
    Inner,
    LeftOuter,
    RightOuter,
    FullOuter,
    Cross,
    Natural,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum JoinConstraint {
    On(ConditionExpression),
    Using(Vec<Column>),
}
