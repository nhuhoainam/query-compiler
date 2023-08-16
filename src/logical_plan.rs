use crate::{
    column::Column,
    common::{FieldDefinitionExpression, FieldValueExpression},
    condition::ConditionExpression,
    join::{JoinClause, JoinCondition, JoinOperator},
    order::OrderByClause,
    select::{GroupByClause, SelectStatement},
    table::Table,
};

pub struct Disticnt {
    pub relation: Relation,
}

pub struct Selection {
    pub relation: Relation,
    pub condition: RelationalCondition,
}

pub struct Projection {
    pub relation: Relation,
    pub values: Vec<ProjectionValue>,
}

pub enum ProjectionValue {
    Column(Column),
    Expression(FieldValueExpression),
}

pub struct Rename {
    pub relation: Relation,
    pub alias: String,
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
    Table(Table),
    Selection(Box<Selection>),
    Projection(Box<Projection>),
    Rename(Box<Rename>),
    Join(
        Box<Relation>,
        Box<Relation>,
        JoinOperator,
        Option<RelationalCondition>,
    ),
    Union(Box<Relation>, Box<Relation>),
    Difference(Box<Relation>, Box<Relation>),
    Intersection(Box<Relation>, Box<Relation>),
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

impl From<FieldDefinitionExpression> for ProjectionValue {
    fn from(value: FieldDefinitionExpression) -> Self {
        match value {
            FieldDefinitionExpression::All => todo!(),
            FieldDefinitionExpression::AllFromTable(_) => todo!(),
            FieldDefinitionExpression::Column(col) => ProjectionValue::Column(col),
            FieldDefinitionExpression::FieldValue(val) => match val {
                FieldValueExpression::Arithmetic(ari) => {
                    ProjectionValue::Expression(FieldValueExpression::Arithmetic(ari))
                }
                FieldValueExpression::Literal(lit) => {
                    ProjectionValue::Expression(FieldValueExpression::Literal(lit))
                }
            },
        }
    }
}

impl From<(Vec<FieldDefinitionExpression>, Relation)> for Relation {
    fn from(value: (Vec<FieldDefinitionExpression>, Relation)) -> Self {
        match value.0.len() {
            0 => value.1,
            _ => {
                let values = value.0.into_iter().map(|x| x.into()).collect();
                Relation::Projection(Box::new(Projection {
                    relation: value.1,
                    values,
                }))
            }
        }
        // let value = value.into_iter().fold(, f)
    }
}

impl From<JoinCondition> for RelationalCondition {
    fn from(value: JoinCondition) -> Self {
        todo!()
    }
}

impl From<ConditionExpression> for RelationalCondition {
    fn from(value: ConditionExpression) -> Self {
        todo!()
    }
}

impl From<(Vec<Table>, Vec<JoinClause>, Option<ConditionExpression>)> for Relation {
    fn from(value: (Vec<Table>, Vec<JoinClause>, Option<ConditionExpression>)) -> Self {
        let first = value.0[0].clone();
        let from = value
            .0
            .into_iter()
            .fold(Relation::Table(first), |acc, table| {
                Relation::Join(
                    Box::new(acc),
                    Box::new(Relation::Table(table)),
                    JoinOperator::Cross,
                    None,
                )
            });

        let join = value.1.into_iter().fold(from, |acc, x| {
            let right_hand = match x.right {
                crate::join::JoinRightHand::Table(ref t) => Relation::Table(t.clone()),
                crate::join::JoinRightHand::Tables(ref ts) => todo!(),
                crate::join::JoinRightHand::NestedSelect(_, _) => todo!(),
                crate::join::JoinRightHand::NestedJoin(_) => todo!(),
            };
            Relation::Join(
                Box::new(acc),
                Box::new(right_hand),
                x.operator,
                Some(x.constraint.into()),
            )
        });

        Relation::Selection(Box::new(Selection {
            relation: join,
            condition: value.2.unwrap().into(),
        }))
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
