use core::fmt;

use crate::{
    column::Column,
    common::{FieldDefinitionExpression, FieldValueExpression, Operator},
    condition::{ConditionBase, ConditionExpression, ConditionTree},
    join::{JoinClause, JoinCondition, JoinOperator, JoinRightHand},
    order::{OrderByClause, OrderType},
    select::{GroupByClause, SelectStatement},
    table::Table,
};

pub trait LogicalPlan {
    fn schema(&self) -> Vec<(String, Vec<Column>)>;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Disticnt {
    pub relation: Relation,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Selection {
    pub relation: Relation,
    pub condition: RelationalCondition,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Projection {
    pub relation: Relation,
    pub values: ProjectionList,
}

pub type ProjectionList = Vec<ProjectionValue>;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ProjectionValue {
    Column(Column),
    Expression(FieldValueExpression),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Rename {
    pub relation: Relation,
    pub alias: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Grouping {
    pub relation: Relation,
    pub columns: Vec<Column>,
    pub condition: Option<RelationalCondition>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Order {
    pub relation: Relation,
    pub columns: Vec<(Column, OrderType)>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct RelationBase {
    pub name: String,
    pub columns: Vec<Column>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Relation {
    Base(RelationBase),
    Selection(Box<Selection>),
    Projection(Box<Projection>),
    Rename(Box<Rename>),
    Join(
        Box<Relation>,
        Box<Relation>,
        JoinOperator,
        Option<RelationalCondition>,
    ),
    Grouping(Box<Grouping>),
    Order(Box<Order>),
    Disticnt(Box<Disticnt>),
    Union(Box<Relation>, Box<Relation>),
    Difference(Box<Relation>, Box<Relation>),
    Intersection(Box<Relation>, Box<Relation>),
}

impl fmt::Display for Relation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Relation::Base(_) => todo!(),
            Relation::Selection(_) => todo!(),
            Relation::Projection(_) => todo!(),
            Relation::Rename(_) => todo!(),
            Relation::Join(_, _, _, _) => todo!(),
            Relation::Grouping(_) => todo!(),
            Relation::Disticnt(_) => todo!(),
            Relation::Union(_, _) => todo!(),
            Relation::Difference(_, _) => todo!(),
            Relation::Intersection(_, _) => todo!(),
            Relation::Order(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct RelationalCondition {
    pub condition: ConditionExpression,
    pub relation: Box<Relation>,
}

impl LogicalPlan for Relation {
    fn schema(&self) -> Vec<(String, Vec<Column>)> {
        match self {
            Relation::Base(base) => vec![(base.name.clone(), base.columns.clone())],
            Relation::Selection(selection) => selection.relation.schema(),
            Relation::Projection(projection) => projection.relation.schema(),
            Relation::Rename(rename) => rename.relation.schema(),
            Relation::Join(left, right, _, _) => {
                let mut left = left.schema();
                let mut right = right.schema();
                left.append(&mut right);
                left
            }
            Relation::Union(left, right) => {
                let mut left = left.schema();
                let mut right = right.schema();
                left.append(&mut right);
                left
            }
            Relation::Difference(left, right) => {
                let mut left = left.schema();
                let mut right = right.schema();
                left.append(&mut right);
                left
            }
            Relation::Intersection(left, right) => {
                let mut left = left.schema();
                let mut right = right.schema();
                left.append(&mut right);
                left
            }
            Relation::Grouping(group) => group.relation.schema(),
            Relation::Disticnt(rel) => rel.relation.schema(),
            Relation::Order(ord) => ord.relation.schema(),
        }
    }
}

impl From<SelectStatement> for Relation {
    fn from(value: SelectStatement) -> Self {
        let from: Relation = (
            value.tables,
            value.join,
            match value.condition {
                Some(condition) => Some(condition.into()),
                None => None,
            },
        )
            .into();
        let projection = (value.sel_list, from.clone()).into();
        let projection: Projection = match projection {
            Relation::Projection(projection) => *projection,
            _ => panic!("projection error"),
        };
        let group: Relation = match value.group_by {
            Some(group) => (group, projection, from).into(),
            None => from,
        };
        let order = match value.order_by {
            Some(order) => (order, group).into(),
            None => group,
        };
        let distinct = match value.distinct {
            true => Relation::Disticnt(Box::new(Disticnt { relation: order })),
            false => order,
        };
        distinct
    }
}

impl From<(Vec<FieldDefinitionExpression>, Relation)> for Relation {
    fn from(value: (Vec<FieldDefinitionExpression>, Relation)) -> Self {
        match value.0.len() {
            0 => value.1,
            _ => {
                let mut values: Vec<ProjectionValue> = vec![];
                value.0.into_iter().for_each(|x| match x {
                    FieldDefinitionExpression::All => (),
                    FieldDefinitionExpression::AllFromTable(t) => t
                        .metadata
                        .unwrap()
                        .into_iter()
                        .for_each(|x| values.push(ProjectionValue::Column(x))),
                    FieldDefinitionExpression::Column(col) => {
                        values.push(ProjectionValue::Column(col))
                    }
                    FieldDefinitionExpression::FieldValue(val) => {
                        let f = match val {
                            FieldValueExpression::Arithmetic(ari) => {
                                ProjectionValue::Expression(FieldValueExpression::Arithmetic(ari))
                            }
                            FieldValueExpression::Literal(lit) => {
                                ProjectionValue::Expression(FieldValueExpression::Literal(lit))
                            }
                        };
                        values.push(f)
                    }
                });
                Relation::Projection(Box::new(Projection {
                    relation: value.1,
                    values,
                }))
            }
        }
    }
}

impl From<(JoinCondition, Relation)> for RelationalCondition {
    fn from(value: (JoinCondition, Relation)) -> Self {
        match value.0 {
            JoinCondition::On(con) => RelationalCondition {
                condition: con,
                relation: Box::new(value.1.clone()),
            },
            JoinCondition::Using(cols) => {
                let mut conds: Vec<ConditionExpression> = vec![];
                cols.into_iter().for_each(|col| {
                    conds.push(ConditionExpression::ComparisonOp(ConditionTree {
                        operator: Operator::Equal,
                        left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                            col.clone(),
                        ))),
                        right: Box::new(ConditionExpression::Base(ConditionBase::Field(col))),
                    }))
                });
                let first_cond = conds[0].clone();
                conds.into_iter().fold(
                    RelationalCondition {
                        condition: first_cond,
                        relation: Box::new(value.1.clone()),
                    },
                    |acc, x| RelationalCondition {
                        condition: ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::And,
                            left: Box::new(acc.condition),
                            right: Box::new(x),
                        }),
                        relation: Box::new(value.1.clone()),
                    },
                )
            }
        }
    }
}

impl From<(Vec<Table>, Vec<JoinClause>, Option<ConditionExpression>)> for Relation {
    fn from(value: (Vec<Table>, Vec<JoinClause>, Option<ConditionExpression>)) -> Self {
        let first = value.0[0].clone();
        let from = value.0.into_iter().fold(
            Relation::Base(RelationBase {
                name: first.name,
                columns: first.metadata.unwrap(), // TODO: handle None
            }),
            |acc, table| {
                Relation::Join(
                    Box::new(acc),
                    Box::new(Relation::Base(RelationBase {
                        name: table.name,
                        columns: table.metadata.unwrap(), // TODO: handle None
                    })),
                    JoinOperator::Cross,
                    None,
                )
            },
        );

        let join = value.1.into_iter().fold(from, |acc, x| {
            let right_hand = match x.right {
                JoinRightHand::Table(t) => Relation::Base(RelationBase {
                    name: t.name.clone(),
                    columns: t.metadata.clone().unwrap(), // TODO: handle None
                }),
                JoinRightHand::Tables(ts) => {
                    let first = ts[0].clone();
                    ts.into_iter().fold(
                        Relation::Base(RelationBase {
                            name: first.name,
                            columns: first.metadata.unwrap(), // TODO: handle None
                        }),
                        |acc, table| {
                            Relation::Join(
                                Box::new(acc),
                                Box::new(Relation::Base(RelationBase {
                                    name: table.name,
                                    columns: table.metadata.unwrap(), // TODO: handle None
                                })),
                                JoinOperator::Cross,
                                None,
                            )
                        },
                    )
                }
                JoinRightHand::NestedSelect(_, _) => todo!(),
                JoinRightHand::NestedJoin(inner_join) => todo!(),
            };
            let join_con = (x.constraint, right_hand.clone()).into();
            Relation::Join(
                Box::new(acc),
                Box::new(right_hand),
                x.operator,
                Some(join_con),
            )
        });

        Relation::Selection(Box::new(Selection {
            relation: join.clone(),
            condition: RelationalCondition {
                condition: value.2.unwrap(),
                relation: Box::new(join),
            },
        }))
    }
}

impl From<(GroupByClause, Projection, Relation)> for Relation {
    fn from(value: (GroupByClause, Projection, Relation)) -> Self {
        let mut columns = value.0.columns.clone();
        value.1.values.into_iter().for_each(|x| match x {
            ProjectionValue::Column(col) => columns.push(col),
            ProjectionValue::Expression(_) => (),
        });
        Relation::Grouping(Box::new(Grouping {
            relation: value.2.clone(),
            columns,
            condition: match value.0.having {
                Some(con) => Some(RelationalCondition {
                    condition: con,
                    relation: Box::new(value.2),
                }),
                None => None,
            },
        }))
    }
}

impl From<(OrderByClause, Relation)> for Relation {
    fn from(value: (OrderByClause, Relation)) -> Self {
        Relation::Order(Box::new(Order {
            relation: value.1,
            columns: value.0.columns,
        }))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_simple_select() {
        
    }
}