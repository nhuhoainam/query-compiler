use std::{collections::BTreeMap, fmt};

use debug_tree::TreeBuilder;

use crate::{
    arithmetic::ArithmeticExpression,
    column::Column,
    common::{FieldDefinitionExpression, FieldValueExpression, Literal, Operator, TreeNode},
    compound_select::{CompoundSelectOperator, CompoundSelectStatement},
    condition::{ConditionBase, ConditionExpression, ConditionTree},
    join::{JoinClause, JoinCondition, JoinOperator, JoinRightHand},
    order::{OrderByClause, OrderType},
    schema::Schema,
    select::{GroupByClause, SelectStatement},
    table::Table,
};

pub trait LogicalPlan {
    fn schema(&self) -> Schema;
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
pub struct Join {
    pub left: Relation,
    pub right: Relation,
    pub operator: JoinOperator,
    pub condition: Option<RelationalCondition>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Grouping {
    pub relation: Relation,
    pub columns: Vec<Column>,
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
    Join(Box<Join>),
    Grouping(Box<Grouping>),
    Order(Box<Order>),
    Disticnt(Box<Disticnt>),
    Union(Box<Relation>, Box<Relation>),
    Difference(Box<Relation>, Box<Relation>),
    Intersection(Box<Relation>, Box<Relation>),
}

impl fmt::Display for Relation {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Relation::Base(_) => todo!(),
            Relation::Selection(_) => todo!(),
            Relation::Projection(_) => todo!(),
            Relation::Rename(_) => todo!(),
            Relation::Join(_) => todo!(),
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
pub enum RelationalConditionBase {
    Field(Column),
    Literal(Literal),
    LiteralList(Vec<Literal>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct RelationalConditionTree {
    pub operator: Operator,
    pub left: Box<RelationalConditionExpression>,
    pub right: Box<RelationalConditionExpression>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum RelationalConditionExpression {
    ComparisonOp(RelationalConditionTree),
    LogicalOp(RelationalConditionTree),
    NegationOp(Box<RelationalConditionExpression>),
    Base(RelationalConditionBase),
    Arithmetic(Box<ArithmeticExpression>),
    // Bracketed(Box<RelationalConditionExpression>),
    None,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct RelationalCondition {
    pub condition: RelationalConditionExpression,
    pub schema: Schema,
}

impl LogicalPlan for Relation {
    fn schema(&self) -> Schema {
        match self {
            Relation::Base(base) => {
                let mut map = BTreeMap::new();
                map.insert(base.name.clone(), base.columns.clone());
                map
            }
            Relation::Selection(selection) => selection.relation.schema(),
            Relation::Projection(projection) => projection.relation.schema(),
            Relation::Rename(rename) => {
                let mut inner = rename.relation.schema();
                match inner.len() {
                    0 => panic!("no table"),
                    1 => {
                        let value = inner.values().next().unwrap().clone();
                        inner.insert(rename.alias.clone(), value);
                        inner
                    }
                    _ => unimplemented!("rename more than one table"),
                }
            }
            Relation::Join(join) => {
                let mut left = join.left.schema();
                let mut right = join.right.schema();
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

impl From<(CompoundSelectStatement, Schema)> for Relation {
    fn from(value: (CompoundSelectStatement, Schema)) -> Self {
        match value.0.selects.len() {
            0 => panic!("no select"),
            1 => (value.0.selects[0].1.clone(), value.1.clone()).into(),
            _ => {
                let first = value.0.selects[0].clone();
                value.0.selects.into_iter().skip(1).fold(
                    (first.1, value.1.clone()).into(),
                    |acc, x| {
                        let left = acc.clone();
                        let right = (x.1, value.1.clone()).into();
                        match x.0 {
                            Some(op) => match op {
                                CompoundSelectOperator::Union => {
                                    Relation::Union(Box::new(left), Box::new(right))
                                }
                                CompoundSelectOperator::Except => {
                                    Relation::Difference(Box::new(left), Box::new(right))
                                }
                                CompoundSelectOperator::Intersect => {
                                    Relation::Intersection(Box::new(left), Box::new(right))
                                }
                                CompoundSelectOperator::DistinctUnion => {
                                    Relation::Disticnt(Box::new(Disticnt {
                                        relation: Relation::Union(Box::new(left), Box::new(right)),
                                    }))
                                }
                            },
                            None => todo!(),
                        }
                    },
                )
            }
        }
    }
}

impl From<(SelectStatement, Schema)> for Relation {
    fn from(value: (SelectStatement, Schema)) -> Self {
        let from: Relation = (
            value.0.tables,
            value.0.join,
            match value.0.condition {
                Some(condition) => Some(condition.into()),
                None => None,
            },
            value.1.clone(),
        )
            .into();
        let projection = (value.0.sel_list, from.clone()).into();
        let projection: Projection = match projection {
            Relation::Projection(projection) => *projection,
            _ => panic!("projection error"),
        };
        let group: Relation = match value.0.group_by {
            Some(group) => (group, projection, from).into(),
            None => Relation::Projection(Box::new(projection)),
        };
        let order = match value.0.order_by {
            Some(order) => (order, group).into(),
            None => group,
        };
        let distinct = match value.0.distinct {
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
                    FieldDefinitionExpression::All => {
                        let schema = value.1.schema();
                        schema.into_iter().for_each(|(_, v)| {
                            v.into_iter()
                                .for_each(|x| values.push(ProjectionValue::Column(x)))
                        })
                    }
                    FieldDefinitionExpression::AllFromTable(t) => {
                        let schema = value.1.schema();
                        if schema.contains_key(&t.name) {
                            let columns = schema.get(&t.name).unwrap().clone();
                            columns
                                .into_iter()
                                .for_each(|x| values.push(ProjectionValue::Column(x)));
                        } else {
                            panic!("no such table: {}", t.name)
                        }
                    }
                    FieldDefinitionExpression::Column(ref col) => {
                        let schema = value.1.schema();
                        let mut found = false;
                        for (t, v) in schema {
                            for c in v {
                                if c.name == col.name || col.name == format!("{}.{}", t, c.name) {
                                    values.push(ProjectionValue::Column(col.to_owned()));
                                    found = true;
                                    break;
                                }
                            }
                            if found {
                                break;
                            }
                        }
                        if !found {
                            panic!("no such column: {}", col.name)
                        }
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

impl From<(JoinCondition, Relation, Schema)> for RelationalCondition {
    fn from(value: (JoinCondition, Relation, Schema)) -> Self {
        match value.0 {
            JoinCondition::On(con) => RelationalCondition {
                condition: (con, value.1).into(),
                schema: value.2,
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
                conds.into_iter().skip(1).fold(
                    RelationalCondition {
                        condition: (first_cond, value.1.clone()).into(),
                        schema: value.2.clone(),
                    },
                    |acc, x| RelationalCondition {
                        condition: RelationalConditionExpression::LogicalOp(
                            RelationalConditionTree {
                                operator: Operator::And,
                                left: Box::new(acc.condition),
                                right: Box::new((x, value.1.clone()).into()),
                            },
                        ),
                        schema: value.2.clone(),
                    },
                )
            }
        }
    }
}

impl From<(ConditionExpression, Relation)> for RelationalConditionExpression {
    fn from(value: (ConditionExpression, Relation)) -> Self {
        match value.0 {
            ConditionExpression::ComparisonOp(cond_tree) => {
                RelationalConditionExpression::ComparisonOp(RelationalConditionTree {
                    operator: cond_tree.operator,
                    left: Box::new((*cond_tree.left, value.1.clone()).into()),
                    right: Box::new((*cond_tree.right, value.1).into()),
                })
            }
            ConditionExpression::LogicalOp(cond_tree) => {
                RelationalConditionExpression::LogicalOp(RelationalConditionTree {
                    operator: cond_tree.operator,
                    left: Box::new((*cond_tree.left, value.1.clone()).into()),
                    right: Box::new((*cond_tree.right, value.1).into()),
                })
            }
            ConditionExpression::NegationOp(inner) => {
                RelationalConditionExpression::NegationOp(Box::new((*inner, value.1).into()))
            }
            ConditionExpression::ExistsOp(_) => RelationalConditionExpression::None,
            ConditionExpression::Base(b) => match b {
                ConditionBase::Field(col) => {
                    RelationalConditionExpression::Base(RelationalConditionBase::Field(col))
                }
                ConditionBase::Literal(lit) => {
                    RelationalConditionExpression::Base(RelationalConditionBase::Literal(lit))
                }
                ConditionBase::LiteralList(lits) => {
                    RelationalConditionExpression::Base(RelationalConditionBase::LiteralList(lits))
                }
                ConditionBase::NestedSelect(_) => RelationalConditionExpression::None,
            },
            ConditionExpression::Arithmetic(ari) => RelationalConditionExpression::Arithmetic(ari),
            ConditionExpression::Bracketed(br) => (*br, value.1).into(),
        }
    }
}

impl From<(ConditionExpression, Relation, RelationalCondition, Schema)> for Selection {
    fn from(value: (ConditionExpression, Relation, RelationalCondition, Schema)) -> Self {
        match value.0 {
            ConditionExpression::ComparisonOp(cond_tree) => Selection {
                relation: value.1.clone(),
                condition: RelationalCondition {
                    condition: RelationalConditionExpression::ComparisonOp(
                        RelationalConditionTree {
                            operator: cond_tree.operator,
                            left: Box::new((*cond_tree.left, value.1.clone()).into()),
                            right: Box::new((*cond_tree.right, value.1.clone()).into()),
                        },
                    ),
                    schema: value.3,
                },
            },
            ConditionExpression::LogicalOp(cond_tree) => {
                let left: RelationalConditionExpression =
                    (*cond_tree.left.clone(), value.1.clone()).into();
                let right: RelationalConditionExpression =
                    (*cond_tree.right.clone(), value.1.clone()).into();
                let left_rel: Option<Relation> = match left {
                    RelationalConditionExpression::None => Some(
                        Selection::from((
                            *cond_tree.left.clone(),
                            value.1.clone(),
                            value.2.clone(),
                            value.3.clone(),
                        ))
                        .relation,
                    ),
                    _ => None,
                };
                let right_rel: Option<Relation> = match right {
                    RelationalConditionExpression::None => Some(
                        Selection::from((
                            *cond_tree.right.clone(),
                            value.1.clone(),
                            value.2.clone(),
                            value.3.clone(),
                        ))
                        .relation,
                    ),
                    _ => None,
                };
                match (left_rel, right_rel) {
                    (None, None) => Selection {
                        relation: value.1.clone(),
                        condition: RelationalCondition {
                            condition: RelationalConditionExpression::LogicalOp(
                                RelationalConditionTree {
                                    operator: cond_tree.operator,
                                    left: Box::new((*cond_tree.left, value.1.clone()).into()),
                                    right: Box::new((*cond_tree.right, value.1.clone()).into()),
                                },
                            ),
                            schema: value.3,
                        },
                    },
                    (None, Some(r)) => Selection {
                        relation: r,
                        condition: RelationalCondition {
                            condition: left,
                            schema: value.3,
                        },
                    },
                    (Some(l), None) => Selection {
                        relation: l,
                        condition: RelationalCondition {
                            condition: right,
                            schema: value.3,
                        },
                    },
                    (Some(l), Some(r)) => Selection {
                        relation: Relation::Join(Box::new(Join {
                            left: l,
                            right: r,
                            operator: JoinOperator::Natural,
                            condition: None,
                        })),
                        condition: RelationalCondition {
                            condition: RelationalConditionExpression::None,
                            schema: value.3,
                        },
                    },
                }
            }
            ConditionExpression::NegationOp(inner) => {
                let inner_cond: RelationalConditionExpression =
                    (*inner.clone(), value.1.clone()).into();
                match inner_cond {
                    RelationalConditionExpression::None => {
                        Selection::from((*inner, value.1.clone(), value.2.clone(), value.3.clone()))
                            .relation
                    }
                    _ => todo!(),
                };
                todo!()
            }
            ConditionExpression::ExistsOp(nested) => {
                let rel = value.1.clone();
                let select: Relation = (*nested, value.3.clone()).into();
                match rel {
                    Relation::Selection(sel) => Selection {
                        relation: Relation::Join(Box::new(Join {
                            left: sel.relation,
                            right: select,
                            operator: JoinOperator::Natural,
                            condition: None,
                        })),
                        condition: sel.condition,
                    },
                    _ => Selection {
                        relation: Relation::Join(Box::new(Join {
                            left: rel,
                            right: select,
                            operator: JoinOperator::Natural,
                            condition: None,
                        })),
                        condition: RelationalCondition {
                            condition: RelationalConditionExpression::None,
                            schema: value.3,
                        },
                    },
                }
            }
            ConditionExpression::Base(b) => match b {
                ConditionBase::Field(c) => Selection {
                    relation: value.1.clone(),
                    condition: RelationalCondition {
                        condition: (
                            ConditionExpression::Base(ConditionBase::Field(c)),
                            value.1.clone(),
                        )
                            .into(),
                        schema: value.3,
                    },
                },
                ConditionBase::Literal(lit) => Selection {
                    relation: value.1.clone(),
                    condition: RelationalCondition {
                        condition: (
                            ConditionExpression::Base(ConditionBase::Literal(lit)),
                            value.1.clone(),
                        )
                            .into(),
                        schema: value.3,
                    },
                },
                ConditionBase::LiteralList(lits) => Selection {
                    relation: value.1.clone(),
                    condition: RelationalCondition {
                        condition: (
                            ConditionExpression::Base(ConditionBase::LiteralList(lits)),
                            value.1.clone(),
                        )
                            .into(),
                        schema: value.3,
                    },
                },
                ConditionBase::NestedSelect(sel) => Selection {
                    relation: (*sel, value.3).into(),
                    condition: value.2,
                },
            },
            ConditionExpression::Arithmetic(ari) => Selection {
                relation: value.1.clone(),
                condition: RelationalCondition {
                    condition: RelationalConditionExpression::Arithmetic(ari),
                    schema: value.3,
                },
            },
            ConditionExpression::Bracketed(br) => (*br, value.1, value.2, value.3).into(),
        }
    }
}

impl
    From<(
        Vec<Table>,
        Vec<JoinClause>,
        Option<ConditionExpression>,
        Schema,
    )> for Relation
{
    fn from(
        value: (
            Vec<Table>,
            Vec<JoinClause>,
            Option<ConditionExpression>,
            Schema,
        ),
    ) -> Self {
        let mut schema = value.3.clone();
        let from = match value.0.len() {
            0 => panic!("no table"),
            1 => {
                match value.0[0].alias {
                    Some(ref a) => {
                        schema.insert(a.clone(), schema.get(&value.0[0].name).unwrap().clone()); // TODO: handle None
                        Relation::Rename(Box::new(Rename {
                            relation: Relation::Base(RelationBase {
                                name: value.0[0].name.clone(),
                                columns: schema.get(&value.0[0].name).unwrap().clone(), // TODO: handle None
                            }),
                            alias: a.clone(),
                        }))
                    }
                    None => Relation::Base(RelationBase {
                        name: value.0[0].name.clone(),
                        columns: match schema.get(&value.0[0].name) {
                            Some(cols) => cols.clone(),
                            None => panic!("no such table: {}", value.0[0].name),
                        }, // TODO: handle None
                    }),
                }
            }
            _ => {
                let first = value.0[0].clone();
                match first.alias {
                    Some(a) => {
                        schema.insert(a.clone(), schema.get(&first.name).unwrap().clone());
                    }
                    None => (),
                }
                value.0.into_iter().skip(1).fold(
                    Relation::Base(RelationBase {
                        name: first.name.clone(),
                        columns: schema.get(&first.name).unwrap().clone(), // TODO: handle None
                    }),
                    |acc, table| {
                        match table.alias {
                            Some(a) => {
                                schema.insert(a.clone(), schema.get(&table.name).unwrap().clone());
                            }
                            None => (),
                        }
                        Relation::Join(Box::new(Join {
                            left: acc,
                            right: Relation::Base(RelationBase {
                                name: table.name.clone(),
                                columns: schema.get(&table.name).unwrap().clone(), // TODO: handle None
                            }),
                            operator: JoinOperator::Cross,
                            condition: None,
                        }))
                    },
                )
            }
        };
        let join = value.1.into_iter().fold(from, |acc, x| {
            let right_hand = match x.right {
                JoinRightHand::Table(t) => {
                    match t.alias {
                        Some(a) => {
                            schema.insert(a.clone(), schema.get(&t.name).unwrap().clone());
                        }
                        None => (),
                    }
                    Relation::Base(RelationBase {
                        name: t.name.clone(),
                        columns: schema.get(&t.name).unwrap().clone(), // TODO: handle None
                    })
                }
                JoinRightHand::Tables(ts) => {
                    let first = ts[0].clone();
                    ts.into_iter().skip(1).fold(
                        Relation::Base(RelationBase {
                            name: first.name.clone(),
                            columns: schema.get(&first.name).unwrap().clone(), // TODO: handle None
                        }),
                        |acc, table| {
                            match table.alias {
                                Some(a) => {
                                    schema.insert(
                                        a.clone(),
                                        schema.get(&table.name).unwrap().clone(),
                                    );
                                }
                                None => (),
                            }
                            Relation::Join(Box::new(Join {
                                left: acc,
                                right: Relation::Base(RelationBase {
                                    name: table.name.clone(),
                                    columns: schema.get(&table.name).unwrap().clone(), // TODO: handle None
                                }),
                                operator: JoinOperator::Cross,
                                condition: None,
                            }))
                        },
                    )
                }
                JoinRightHand::NestedSelect(sel, alias) => {
                    let rel: Relation = (*sel, schema.clone()).into();
                    match alias {
                        Some(a) => {
                            for (t, cols) in rel.schema() {
                                schema.insert(t, cols);
                            }
                            Relation::Rename(Box::new(Rename {
                                relation: rel,
                                alias: a,
                            }))
                        }
                        None => rel,
                    }
                }
                JoinRightHand::NestedJoin(_) => todo!(),
            };
            let join_con = (x.constraint, right_hand.clone(), schema.clone()).into();
            Relation::Join(Box::new(Join {
                left: acc,
                right: right_hand,
                operator: x.operator,
                condition: Some(join_con),
            }))
        });

        match value.2 {
            Some(condition) => Relation::Selection(Box::new(
                (
                    condition,
                    join,
                    RelationalCondition {
                        condition: RelationalConditionExpression::None,
                        schema: schema.clone(),
                    },
                    schema,
                )
                    .into(),
            )),
            None => join,
        }
    }
}

impl fmt::Display for RelationalConditionBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            RelationalConditionBase::Field(c) => write!(f, "{}", c),
            RelationalConditionBase::Literal(l) => write!(f, "{}", l),
            RelationalConditionBase::LiteralList(l) => {
                write!(
                    f,
                    "({})",
                    l.iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

impl TreeNode for RelationalConditionBase {
    fn populate(&self, parent: &TreeBuilder) {
        match self {
            RelationalConditionBase::Field(c) => c.populate(parent),
            RelationalConditionBase::Literal(l) => parent.add_leaf(format!("{}", l).as_str()),
            RelationalConditionBase::LiteralList(l) => {
                let mut branch = parent.add_branch("()");
                for lit in l.iter() {
                    parent.add_leaf(format!("{}", lit).as_str());
                }
                branch.release();
            }
        }
    }
}

impl TreeNode for RelationalConditionExpression {
    fn populate(&self, parent: &TreeBuilder) {
        match &self {
            RelationalConditionExpression::ComparisonOp(cond_tree) => {
                let mut branch = parent.add_branch(&format!("{}", cond_tree.operator));
                cond_tree.left.populate(parent);
                cond_tree.right.populate(parent);
                branch.release();
            }
            RelationalConditionExpression::LogicalOp(cond_tree) => {
                let mut branch = parent.add_branch(&format!("{}", cond_tree.operator));
                cond_tree.left.populate(parent);
                cond_tree.right.populate(parent);
                branch.release();
            }
            RelationalConditionExpression::NegationOp(inner) => {
                let mut branch = parent.add_branch("NOT");
                inner.populate(parent);
                branch.release();
            }
            RelationalConditionExpression::Base(base) => base.populate(parent),
            RelationalConditionExpression::Arithmetic(exp) => exp.populate(parent),
            // RelationalConditionExpression::Bracketed(inner) => {
            //     let mut branch = parent.add_branch("()");
            //     inner.populate(parent);
            //     branch.release();
            // }
            RelationalConditionExpression::None => todo!(),
        }
    }
}

impl fmt::Display for RelationalConditionTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

impl fmt::Display for RelationalConditionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RelationalConditionExpression::ComparisonOp(c) => write!(f, "{}", c),
            RelationalConditionExpression::LogicalOp(c) => write!(f, "{}", c),
            RelationalConditionExpression::NegationOp(c) => write!(f, "NOT {}", c),
            RelationalConditionExpression::Base(b) => write!(f, "{}", b),
            RelationalConditionExpression::Arithmetic(a) => write!(f, "{}", a),
            // RelationalConditionExpression::Bracketed(c) => write!(f, "({})", c),
            RelationalConditionExpression::None => Ok(()),
        }
    }
}

impl TreeNode for RelationalCondition {
    fn populate(&self, parent: &TreeBuilder) {
        self.condition.populate(parent)
    }
}

impl TreeNode for ProjectionValue {
    fn populate(&self, parent: &TreeBuilder) {
        match &self {
            ProjectionValue::Column(col) => col.populate(parent),
            ProjectionValue::Expression(exp) => exp.populate(parent),
        }
    }
}

impl fmt::Display for ProjectionValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ProjectionValue::Column(col) => {
                match &col.table {
                    Some(t) => write!(f, "{}.", t)?,
                    None => (),
                }
                write!(f, "{}", col.name)
            }
            ProjectionValue::Expression(exp) => write!(f, "{}", exp),
        }
    }
}

impl TreeNode for Relation {
    fn populate(&self, parent: &TreeBuilder) {
        match &self {
            Relation::Base(base) => {
                let mut branch = parent.add_branch(base.name.as_str());
                // for col in base.columns.iter() {
                //     col.populate(parent);
                // }
                branch.release();
            }
            Relation::Selection(selection) => match selection.condition.condition {
                RelationalConditionExpression::None => selection.relation.populate(parent),
                _ => {
                    let sel = format!("SELECTION {}", selection.condition.condition);
                    let mut branch = parent.add_branch(&sel);
                    selection.relation.populate(parent);
                    branch.release();
                }
            },
            Relation::Projection(projection) => {
                let mut proj = "PROJECTION ".to_string();
                for val in projection.values.iter() {
                    proj = proj + &format!("{} ", val);
                }
                let mut branch = parent.add_branch(proj.as_str());
                projection.relation.populate(parent);
                branch.release();
            }
            Relation::Rename(rename) => {
                let mut branch = parent.add_branch(format!("RENAME {}", rename.alias).as_str());
                rename.relation.populate(parent);
                branch.release();
            }
            Relation::Join(j) => {
                let left = &j.left;
                let right = &j.right;
                let con = &j.condition;
                let mut join = format!("{} ", j.operator);
                if let Some(con) = con {
                    join = join + &con.condition.to_string();
                }
                let mut branch = parent.add_branch(&join);
                left.populate(parent);
                right.populate(parent);
                branch.release();
            }
            Relation::Grouping(group) => {
                let mut cols = "GROUPING ".to_string();
                for col in group.columns.iter() {
                    cols = cols + &format!("{} ", col.name);
                }
                let mut branch = parent.add_branch(&cols);
                group.relation.populate(parent);
                branch.release();
            }
            Relation::Disticnt(dist) => {
                let mut branch = parent.add_branch("DISTINCT");
                dist.relation.populate(parent);
                branch.release();
            }
            Relation::Union(left, right) => {
                let mut branch = parent.add_branch("UNION");
                left.populate(parent);
                right.populate(parent);
                branch.release();
            }
            Relation::Difference(left, right) => {
                let mut branch = parent.add_branch("DIFFERENCE");
                left.populate(parent);
                right.populate(parent);
                branch.release();
            }
            Relation::Intersection(left, right) => {
                let mut branch = parent.add_branch("INTERSECTION");
                left.populate(parent);
                right.populate(parent);
                branch.release();
            }
            Relation::Order(ord) => {
                let mut branch = parent.add_branch("ORDER");
                ord.relation.populate(parent);
                for col in ord.columns.iter() {
                    col.0.populate(parent);
                }
                branch.release();
            }
        }
    }
}

impl From<(GroupByClause, Projection, Relation)> for Relation {
    fn from(value: (GroupByClause, Projection, Relation)) -> Self {
        let mut columns = value.0.columns.clone();
        value.1.values.into_iter().for_each(|x| match x {
            ProjectionValue::Column(col) => columns.push(col),
            ProjectionValue::Expression(_) => (),
        });
        match value.0.having {
            Some(con) => Relation::Selection(Box::new(Selection {
                relation: Relation::Grouping(Box::new(Grouping {
                    relation: value.2.clone(),
                    columns,
                })),
                condition: RelationalCondition {
                    condition: (con, value.2.clone()).into(),
                    schema: value.2.schema(),
                },
            })),
            None => Relation::Grouping(Box::new(Grouping {
                relation: value.2.clone(),
                columns,
            })),
        }
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
    use super::*;
    use crate::{common::Literal, select::select_statement};

    fn get_schema() -> Schema {
        let mut schema: Schema = BTreeMap::new();
        schema.insert(
            "table1".to_string(),
            vec![
                Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                },
                Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                },
            ],
        );
        schema.insert(
            "table2".to_string(),
            vec![
                Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                },
                Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                },
            ],
        );
        schema
    }

    #[test]
    fn test_simple_select_all() {
        let sql = b"SELECT * FROM table1";
        let parse_tree = select_statement(sql).unwrap().1;
        let schema = get_schema();
        let logical_plan: Relation = (parse_tree, schema.clone()).into();
        let expected = Relation::Projection(Box::new(Projection {
            relation: Relation::Base(RelationBase {
                name: "table1".to_string(),
                columns: vec![
                    Column {
                        name: "column1".to_string(),
                        alias: None,
                        table: None,
                        function: None,
                    },
                    Column {
                        name: "column2".to_string(),
                        alias: None,
                        table: None,
                        function: None,
                    },
                ],
            }),
            values: vec![
                ProjectionValue::Column(Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
            ],
        }));
        assert_eq!(logical_plan, expected);
    }

    #[test]
    fn test_simple_select() {
        let sql = b"SELECT column1 FROM table1";
        let parse_tree = select_statement(sql).unwrap().1;
        let schema = get_schema();
        let logical_plan: Relation = (parse_tree, schema.clone()).into();
        let expected = Relation::Projection(Box::new(Projection {
            relation: Relation::Base(RelationBase {
                name: "table1".to_string(),
                columns: vec![
                    Column {
                        name: "column1".to_string(),
                        alias: None,
                        table: None,
                        function: None,
                    },
                    Column {
                        name: "column2".to_string(),
                        alias: None,
                        table: None,
                        function: None,
                    },
                ],
            }),
            values: vec![ProjectionValue::Column(Column {
                name: "column1".to_string(),
                alias: None,
                table: None,
                function: None,
            })],
        }));
        assert_eq!(logical_plan, expected);

        let sql = b"SELECT column1, column2 FROM table1 WHERE column1 = 1";
        let parse_tree = select_statement(sql).unwrap().1;
        let logical_plan: Relation = (parse_tree, schema.clone()).into();
        let expected = Relation::Projection(Box::new(Projection {
            relation: Relation::Selection(Box::new(Selection {
                relation: Relation::Base(RelationBase {
                    name: "table1".to_string(),
                    columns: vec![
                        Column {
                            name: "column1".to_string(),
                            alias: None,
                            table: None,
                            function: None,
                        },
                        Column {
                            name: "column2".to_string(),
                            alias: None,
                            table: None,
                            function: None,
                        },
                    ],
                }),
                condition: RelationalCondition {
                    condition: RelationalConditionExpression::ComparisonOp(
                        RelationalConditionTree {
                            operator: Operator::Equal,
                            left: Box::new(RelationalConditionExpression::Base(
                                RelationalConditionBase::Field(Column {
                                    name: "column1".to_string(),
                                    alias: None,
                                    table: None,
                                    function: None,
                                }),
                            )),
                            right: Box::new(RelationalConditionExpression::Base(
                                RelationalConditionBase::Literal(Literal::Integer(1)),
                            )),
                        },
                    ),
                    schema: schema.clone(),
                },
            })),
            values: vec![
                ProjectionValue::Column(Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
            ],
        }));
        assert_eq!(logical_plan, expected);
    }

    #[test]
    fn test_simple_join() {
        let sql = b"SELECT * FROM table1, table2";
        let parse_tree = select_statement(sql).unwrap().1;
        let schema = get_schema();
        let logical_plan: Relation = (parse_tree, schema.clone()).into();
        let expected = Relation::Projection(Box::new(Projection {
            relation: Relation::Join(Box::new(Join {
                left: Relation::Base(RelationBase {
                    name: "table1".to_string(),
                    columns: vec![
                        Column {
                            name: "column1".to_string(),
                            alias: None,
                            table: None,
                            function: None,
                        },
                        Column {
                            name: "column2".to_string(),
                            alias: None,
                            table: None,
                            function: None,
                        },
                    ],
                }),
                right: Relation::Base(RelationBase {
                    name: "table2".to_string(),
                    columns: vec![
                        Column {
                            name: "column1".to_string(),
                            alias: None,
                            table: None,
                            function: None,
                        },
                        Column {
                            name: "column2".to_string(),
                            alias: None,
                            table: None,
                            function: None,
                        },
                    ],
                }),
                operator: JoinOperator::Cross,
                condition: None,
            })),
            values: vec![
                ProjectionValue::Column(Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
            ],
        }));
        assert_eq!(logical_plan, expected);

        let sql = b"SELECT * FROM table1, table2 WHERE table1.column1 = table2.column1";
        let parse_tree = select_statement(sql).unwrap().1;
        let logical_plan: Relation = (parse_tree, schema.clone()).into();
        let expected = Relation::Projection(Box::new(Projection {
            relation: Relation::Selection(Box::new(Selection {
                relation: Relation::Join(Box::new(Join {
                    left: Relation::Base(RelationBase {
                        name: "table1".to_string(),
                        columns: vec![
                            Column {
                                name: "column1".to_string(),
                                alias: None,
                                table: None,
                                function: None,
                            },
                            Column {
                                name: "column2".to_string(),
                                alias: None,
                                table: None,
                                function: None,
                            },
                        ],
                    }),
                    right: Relation::Base(RelationBase {
                        name: "table2".to_string(),
                        columns: vec![
                            Column {
                                name: "column1".to_string(),
                                alias: None,
                                table: None,
                                function: None,
                            },
                            Column {
                                name: "column2".to_string(),
                                alias: None,
                                table: None,
                                function: None,
                            },
                        ],
                    }),
                    operator: JoinOperator::Cross,
                    condition: None,
                })),
                condition: RelationalCondition {
                    condition: RelationalConditionExpression::ComparisonOp(
                        RelationalConditionTree {
                            operator: Operator::Equal,
                            left: Box::new(RelationalConditionExpression::Base(
                                RelationalConditionBase::Field(Column {
                                    name: "column1".to_string(),
                                    alias: None,
                                    table: Some("table1".to_string()),
                                    function: None,
                                }),
                            )),
                            right: Box::new(RelationalConditionExpression::Base(
                                RelationalConditionBase::Field(Column {
                                    name: "column1".to_string(),
                                    alias: None,
                                    table: Some("table2".to_string()),
                                    function: None,
                                }),
                            )),
                        },
                    ),
                    schema: schema.clone(),
                },
            })),
            values: vec![
                ProjectionValue::Column(Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column1".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ProjectionValue::Column(Column {
                    name: "column2".to_string(),
                    alias: None,
                    table: None,
                    function: None,
                }),
            ],
        }));
        assert_eq!(logical_plan, expected);
    }
}
