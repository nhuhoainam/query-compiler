use crate::column::Column;

pub trait LogicalPlan {
    fn schema(&self) -> Schema;
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