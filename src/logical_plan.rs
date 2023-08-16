use crate::{column::Column, select::{SelectStatement, GroupByClause}, common::FieldDefinitionExpression, table::Table, join::JoinClause, condition::ConditionExpression, order::OrderByClause};

pub struct Disticnt {
    pub relation: Relation,
}

pub struct Selection {
    pub relation: Relation,
    pub condition: RelationalCondition,
}

pub struct Projection {
    pub relation: Relation,
    pub columns: Vec<Column>,
}

pub struct Rename {
    pub relation: Relation,
    pub columns: Vec<Column>,
}

pub struct Grouping {
    pub relation: Relation,
    pub columns: Vec<Column>,
    pub condition: RelationalCondition,
}

pub enum Aggregation {
    Count(Column),
    Sum(Column),
    Average(Column),
    Min(Column),
    Max(Column),
}

pub enum Relation {
    Selection(Box<Selection>),
    Projection(Box<Projection>),
    Rename(Box<Rename>),
    Join(Box<Relation>, Box<Relation>, JoinOperation, RelationalCondition),
    Union(Box<Relation>, Box<Relation>),
    Difference(Box<Relation>, Box<Relation>),
    Intersection(Box<Relation>, Box<Relation>)
}

pub enum JoinOperation {
    Natural,
    Inner,
    LeftOuter,
    RightOuter,
    FullOuter,
    Cross,
}

pub enum RelationalCondition {
    Comparison(Column, Column, ComparisonOperator),
    Conjunction(Box<RelationalCondition>, Box<RelationalCondition>),
    Disjunction(Box<RelationalCondition>, Box<RelationalCondition>),
    Negation(Box<RelationalCondition>),
}

pub enum ComparisonOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl From<SelectStatement> for Relation {
    fn from(value: SelectStatement) -> Self {
        todo!()
    }
}

impl From<Vec<FieldDefinitionExpression>> for Relation {
    fn from(value: Vec<FieldDefinitionExpression>) -> Self {
        todo!()
    }
}
    
impl From<(Vec<Table>, Vec<JoinClause>, Option<ConditionExpression>)> for Relation {
    fn from(value: (Vec<Table>, Vec<JoinClause>, Option<ConditionExpression>)) -> Self {
        todo!()
    }
}

impl From<GroupByClause> for Relation {
    fn from(value: GroupByClause) -> Self {
        todo!()
    }
}

impl From<OrderByClause> for Relation {
    fn from(value: OrderByClause) -> Self {
        todo!()
    }
}