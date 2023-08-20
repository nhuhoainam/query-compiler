use core::fmt;
use std::collections::BTreeMap;

use debug_tree::TreeBuilder;

use crate::{
    column::Column,
    common::{FieldDefinitionExpression, FieldValueExpression, Operator, TreeNode},
    condition::{ConditionBase, ConditionExpression, ConditionTree},
    join::{JoinClause, JoinCondition, JoinOperator, JoinRightHand},
    order::{OrderByClause, OrderType},
    select::{GroupByClause, SelectStatement},
    table::Table, schema::Schema,
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
                    FieldDefinitionExpression::Column(col) => {
                        let schema = value.1.schema();
                        let mut found = false;
                        for (t, v) in schema {
                            for c in v {
                                if c.name == col.name || col.name == format!("{}.{}", t, c.name) {
                                    values.push(ProjectionValue::Column(c));
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

impl From<(JoinCondition, Schema)> for RelationalCondition {
    fn from(value: (JoinCondition, Schema)) -> Self {
        match value.0 {
            JoinCondition::On(con) => RelationalCondition {
                condition: con,
                schema: value.1,
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
                        condition: first_cond,
                        schema: value.1.clone(),
                    },
                    |acc, x| RelationalCondition {
                        condition: ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::And,
                            left: Box::new(acc.condition),
                            right: Box::new(x),
                        }),
                        schema: value.1.clone(),
                    },
                )
            }
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
        let from = match value.0.len() {
            0 => panic!("no table"),
            1 => Relation::Base(RelationBase {
                name: value.0[0].name.clone(),
                columns: value.3.get(&value.0[0].name).unwrap().clone(), // TODO: handle None
            }),
            _ => {
                let first = value.0[0].clone();
                value.0.into_iter().skip(1).fold(
                    Relation::Base(RelationBase {
                        name: first.name.clone(),
                        columns: value.3.get(&first.name).unwrap().clone(), // TODO: handle None
                    }),
                    |acc, table| {
                        Relation::Join(
                            Box::new(acc),
                            Box::new(Relation::Base(RelationBase {
                                name: table.name.clone(),
                                columns: value.3.get(&table.name).unwrap().clone(), // TODO: handle None
                            })),
                            JoinOperator::Cross,
                            None,
                        )
                    },
                )
            }
        };
        let join = value.1.into_iter().fold(from, |acc, x| {
            let right_hand = match x.right {
                JoinRightHand::Table(t) => Relation::Base(RelationBase {
                    name: t.name.clone(),
                    columns: value.3.get(&t.name).unwrap().clone(), // TODO: handle None
                }),
                JoinRightHand::Tables(ts) => {
                    let first = ts[0].clone();
                    ts.into_iter().skip(1).fold(
                        Relation::Base(RelationBase {
                            name: first.name.clone(),
                            columns: value.3.get(&first.name).unwrap().clone(), // TODO: handle None
                        }),
                        |acc, table| {
                            Relation::Join(
                                Box::new(acc),
                                Box::new(Relation::Base(RelationBase {
                                    name: table.name.clone(),
                                    columns: value.3.get(&table.name).unwrap().clone(), // TODO: handle None
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
            let join_con = (x.constraint, right_hand.schema()).into();
            println!("{:#?}", join_con);
            Relation::Join(
                Box::new(acc),
                Box::new(right_hand),
                x.operator,
                Some(join_con),
            )
        });

        match value.2 {
            Some(condition) => Relation::Selection(Box::new(Selection {
                relation: join.clone(),
                condition: RelationalCondition {
                    condition,
                    schema: value.3,
                },
            })),
            None => join,
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
            ProjectionValue::Column(col) => write!(f, "{}", col.name),
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
            Relation::Selection(selection) => {
                let sel = format!("SELECTION {}", selection.condition.condition);
                let mut branch = parent.add_branch(&sel);
                selection.relation.populate(parent);
                branch.release();
            }
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
            Relation::Join(left, right, op, con) => {
                let mut join = format!("{} ", op);
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
                    condition: con,
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
                    condition: ConditionExpression::ComparisonOp(ConditionTree {
                        operator: Operator::Equal,
                        left: Box::new(ConditionExpression::Base(ConditionBase::Field(Column {
                            name: "column1".to_string(),
                            alias: None,
                            table: None,
                            function: None,
                        }))),
                        right: Box::new(ConditionExpression::Base(ConditionBase::Literal(
                            Literal::Integer(1),
                        ))),
                    }),
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
            relation: Relation::Join(
                Box::new(Relation::Base(RelationBase {
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
                })),
                Box::new(Relation::Base(RelationBase {
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
                })),
                JoinOperator::Cross,
                None,
            ),
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
                relation: Relation::Join(
                    Box::new(Relation::Base(RelationBase {
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
                    })),
                    Box::new(Relation::Base(RelationBase {
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
                    })),
                    JoinOperator::Cross,
                    None,
                ),
                condition: RelationalCondition {
                    condition: ConditionExpression::ComparisonOp(ConditionTree {
                        operator: Operator::Equal,
                        left: Box::new(ConditionExpression::Base(ConditionBase::Field(Column {
                            name: "column1".to_string(),
                            alias: None,
                            table: Some("table1".to_string()),
                            function: None,
                        }))),
                        right: Box::new(ConditionExpression::Base(ConditionBase::Field(Column {
                            name: "column1".to_string(),
                            alias: None,
                            table: Some("table2".to_string()),
                            function: None,
                        }))),
                    }),
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
