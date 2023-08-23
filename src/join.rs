use std::fmt;

use debug_tree::TreeBuilder;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{multispace0, multispace1},
    combinator::{map, opt},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

use crate::{
    column::Column,
    common::{as_alias, field_list, opt_delimited, table_list, table_reference, TreeNode},
    condition::{condition_expr, ConditionExpression},
    select::{nested_select_statement, SelectStatement},
    table::Table,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct JoinClause {
    pub operator: JoinOperator,
    pub right: JoinRightHand,
    pub constraint: JoinCondition,
}

impl TreeNode for JoinClause {
    fn populate(&self, parent: &TreeBuilder) {
        let mut branch = parent.add_branch(format!("{}", self.operator).as_str());
        self.right.populate(parent);
        self.constraint.populate(parent);
        branch.release();
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum JoinRightHand {
    Table(Table),
    // A comma-separated (and implicitly joined) sequence of tables.
    Tables(Vec<Table>),
    // A nested selection, represented as (query, alias).
    NestedSelect(Box<SelectStatement>, Option<String>),
    NestedJoin(Box<JoinClause>),
}

impl TreeNode for JoinRightHand {
    fn populate(&self, parent: &TreeBuilder) {
        match self {
            JoinRightHand::Table(ref t) => t.populate(parent),
            JoinRightHand::Tables(ref t) => {
                for table in t.iter() {
                    table.populate(parent);
                }
            }
            JoinRightHand::NestedSelect(ref s, ref a) => match a {
                Some(ref a) => {
                    let mut branch = parent.add_branch(format!("{}", a).as_str());
                    s.populate(parent);
                    branch.release();
                }
                None => s.populate(parent),
            },
            JoinRightHand::NestedJoin(ref j) => {
                j.populate(parent);
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum JoinOperator {
    Inner,
    LeftOuter,
    RightOuter,
    FullOuter,
    Cross,
    Natural,
    Anti,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum JoinCondition {
    On(ConditionExpression),
    Using(Vec<Column>),
}

impl TreeNode for JoinCondition {
    fn populate(&self, parent: &TreeBuilder) {
        match self {
            JoinCondition::On(ref c) => {
                let mut branch = parent.add_branch("ON");
                parent.add_leaf(format!("{}", c).as_str());
                branch.release();
            }
            JoinCondition::Using(ref c) => {
                let mut branch = parent.add_branch("USING");
                for column in c.iter() {
                    column.populate(parent);
                }
                branch.release();
            }
        }
    }
}

impl fmt::Display for JoinRightHand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JoinRightHand::Table(ref t) => write!(f, "{}", t),
            JoinRightHand::Tables(ref t) => write!(
                f,
                "{}",
                t.iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            JoinRightHand::NestedSelect(ref s, ref a) => match a {
                Some(ref a) => write!(f, "({}) AS {}", s, a),
                None => write!(f, "({})", s),
            },
            JoinRightHand::NestedJoin(ref j) => write!(f, "{}", j),
        }
    }
}

impl fmt::Display for JoinCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JoinCondition::On(ref c) => write!(f, "ON {}", c),
            JoinCondition::Using(ref c) => write!(
                f,
                "USING({})",
                c.iter()
                    .map(|c| format!("{}", c))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl fmt::Display for JoinClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.operator)?;
        write!(f, " {}", self.right)?;
        write!(f, " {}", self.constraint)?;
        Ok(())
    }
}

impl fmt::Display for JoinOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JoinOperator::Inner => write!(f, "INNER JOIN"),
            JoinOperator::LeftOuter => write!(f, "LEFT OUTER JOIN"),
            JoinOperator::RightOuter => write!(f, "RIGHT OUTER JOIN"),
            JoinOperator::FullOuter => write!(f, "FULL OUTER JOIN"),
            JoinOperator::Cross => write!(f, "CROSS JOIN"),
            JoinOperator::Natural => write!(f, "NATURAL JOIN"),
            JoinOperator::Anti => write!(f, "ANTI JOIN"),
        }
    }
}

// Parse binary comparison operators
pub fn join_operator(i: &[u8]) -> IResult<&[u8], JoinOperator> {
    alt((
        map(tag_no_case("join"), |_| JoinOperator::Natural),
        map(tag_no_case("left join"), |_| JoinOperator::LeftOuter),
        map(tag_no_case("right join"), |_| JoinOperator::RightOuter),
        map(tag_no_case("inner join"), |_| JoinOperator::Inner),
        map(tag_no_case("cross join"), |_| JoinOperator::Cross),
    ))(i)
}

fn join_constraint(i: &[u8]) -> IResult<&[u8], JoinCondition> {
    let using_clause = map(
        tuple((
            tag_no_case("using"),
            multispace0,
            delimited(
                terminated(tag("("), multispace0),
                field_list,
                preceded(multispace0, tag(")")),
            ),
        )),
        |t| JoinCondition::Using(t.2),
    );
    let on_constraint = alt((
        opt_delimited(
            terminated(tag("("), multispace0),
            condition_expr,
            preceded(multispace0, tag(")")),
        ),
        condition_expr,
    ));
    let on_clause = map(
        tuple((tag_no_case("on"), multispace1, on_constraint)),
        |t| JoinCondition::On(t.2),
    );

    alt((using_clause, on_clause))(i)
}

// Parse JOIN clause
pub fn join_clause(i: &[u8]) -> IResult<&[u8], JoinClause> {
    let (remaining_input, (_, _natural, operator, _, right, _, constraint)) = tuple((
        multispace0,
        opt(terminated(tag_no_case("natural"), multispace1)),
        join_operator,
        multispace1,
        join_rhs,
        multispace1,
        join_constraint,
    ))(i)?;

    Ok((
        remaining_input,
        JoinClause {
            operator,
            right,
            constraint,
        },
    ))
}

fn join_rhs(i: &[u8]) -> IResult<&[u8], JoinRightHand> {
    let nested_select = map(
        tuple((
            delimited(tag("("), nested_select_statement, tag(")")),
            opt(as_alias),
        )),
        |t| JoinRightHand::NestedSelect(Box::new(t.0), t.1.map(String::from)),
    );
    let nested_join = map(delimited(tag("("), join_clause, tag(")")), |nj| {
        JoinRightHand::NestedJoin(Box::new(nj))
    });
    let table = map(table_reference, |t| JoinRightHand::Table(t));
    let tables = map(delimited(tag("("), table_list, tag(")")), |tables| {
        JoinRightHand::Tables(tables)
    });
    alt((nested_select, nested_join, table, tables))(i)
}

#[cfg(test)]
mod tests {
    use crate::{
        common::{FieldDefinitionExpression, Operator},
        condition::{ConditionBase, ConditionTree},
    };

    use super::*;

    #[test]
    fn test_join_operator() {
        assert_eq!(join_operator(b"join"), Ok((&[][..], JoinOperator::Natural)));
        assert_eq!(
            join_operator(b"left join"),
            Ok((&[][..], JoinOperator::LeftOuter))
        );
        assert_eq!(
            join_operator(b"right join"),
            Ok((&[][..], JoinOperator::RightOuter))
        );
        assert_eq!(
            join_operator(b"inner join"),
            Ok((&[][..], JoinOperator::Inner))
        );
        assert_eq!(
            join_operator(b"cross join"),
            Ok((&[][..], JoinOperator::Cross))
        );
    }

    #[test]
    fn test_join_constraint() {
        assert_eq!(
            join_constraint(b"using (a, b, c)"),
            Ok((
                &[][..],
                JoinCondition::Using(vec![
                    Column::from("a"),
                    Column::from("b"),
                    Column::from("c")
                ])
            ))
        );
        assert_eq!(
            join_constraint(b"on a = b"),
            Ok((
                &[][..],
                JoinCondition::On(ConditionExpression::ComparisonOp(ConditionTree {
                    operator: Operator::Equal,
                    left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                        Column::from("a")
                    ))),
                    right: Box::new(ConditionExpression::Base(ConditionBase::Field(
                        Column::from("b")
                    ))),
                }))
            ))
        );
        assert_eq!(
            join_constraint(b"on (a = b)"),
            Ok((
                &[][..],
                JoinCondition::On(ConditionExpression::Bracketed(Box::new(
                    ConditionExpression::ComparisonOp(ConditionTree {
                        operator: Operator::Equal,
                        left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                            Column::from("a")
                        ))),
                        right: Box::new(ConditionExpression::Base(ConditionBase::Field(
                            Column::from("b")
                        ))),
                    })
                )))
            ))
        );
    }

    #[test]
    fn test_join_clause() {
        let sample = join_clause(b"join t on a = b").unwrap();
        assert_eq!(
            sample.1,
            JoinClause {
                operator: JoinOperator::Natural,
                right: JoinRightHand::Table(Table::from("t".to_string())),
                constraint: JoinCondition::On(ConditionExpression::ComparisonOp(ConditionTree {
                    operator: Operator::Equal,
                    left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                        Column::from("a")
                    ))),
                    right: Box::new(ConditionExpression::Base(ConditionBase::Field(
                        Column::from("b")
                    ))),
                }))
            }
        );
        assert_eq!(
            join_clause(b"join t using (a, b, c)"),
            Ok((
                &[][..],
                JoinClause {
                    operator: JoinOperator::Natural,
                    right: JoinRightHand::Table(Table::from("t".to_string())),
                    constraint: JoinCondition::Using(vec![
                        Column::from("a"),
                        Column::from("b"),
                        Column::from("c")
                    ])
                }
            ))
        );
        assert_eq!(
            join_clause(b"join (select * from t) as t2 on a = b"),
            Ok((
                &[][..],
                JoinClause {
                    operator: JoinOperator::Natural,
                    right: JoinRightHand::NestedSelect(
                        Box::new(SelectStatement {
                            distinct: false,
                            sel_list: vec![FieldDefinitionExpression::All],
                            tables: vec![Table::from("t".to_string())],
                            join: vec![],
                            condition: None,
                            group_by: None,
                            order_by: None,
                        }),
                        Some(String::from("t2"))
                    ),
                    constraint: JoinCondition::On(ConditionExpression::ComparisonOp(
                        ConditionTree {
                            operator: Operator::Equal,
                            left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                                Column::from("a")
                            ))),
                            right: Box::new(ConditionExpression::Base(ConditionBase::Field(
                                Column::from("b")
                            ))),
                        }
                    ))
                }
            ))
        );
        assert_eq!(
            join_clause(b"join (select * from t) as t2 using (a, b, c)"),
            Ok((
                &[][..],
                JoinClause {
                    operator: JoinOperator::Natural,
                    right: JoinRightHand::NestedSelect(
                        Box::new(SelectStatement {
                            distinct: false,
                            sel_list: vec![FieldDefinitionExpression::All],
                            tables: vec![Table::from("t".to_string())],
                            join: vec![],
                            condition: None,
                            group_by: None,
                            order_by: None,
                        }),
                        Some(String::from("t2"))
                    ),
                    constraint: JoinCondition::Using(vec![
                        Column::from("a"),
                        Column::from("b"),
                        Column::from("c")
                    ]),
                }
            ))
        );
        assert_eq!(
            join_clause(b"join (select * from t) as t2 using (a, b, c)"),
            Ok((
                &[][..],
                JoinClause {
                    operator: JoinOperator::Natural,
                    right: JoinRightHand::NestedSelect(
                        Box::new(SelectStatement {
                            distinct: false,
                            sel_list: vec![FieldDefinitionExpression::All],
                            tables: vec![Table::from("t".to_string())],
                            join: vec![],
                            condition: None,
                            group_by: None,
                            order_by: None,
                        }),
                        Some(String::from("t2"))
                    ),
                    constraint: JoinCondition::Using(vec![
                        Column::from("a"),
                        Column::from("b"),
                        Column::from("c")
                    ]),
                }
            ))
        );
        assert_eq!(
            join_clause(b"join (select * from t) as t2 using (a, b, c)"),
            Ok((
                &[][..],
                JoinClause {
                    operator: JoinOperator::Natural,
                    right: JoinRightHand::NestedSelect(
                        Box::new(SelectStatement {
                            distinct: false,
                            sel_list: vec![FieldDefinitionExpression::All],
                            tables: vec![Table::from("t".to_string())],
                            join: vec![],
                            condition: None,
                            group_by: None,
                            order_by: None,
                        }),
                        Some(String::from("t2"))
                    ),
                    constraint: JoinCondition::Using(vec![
                        Column::from("a"),
                        Column::from("b"),
                        Column::from("c")
                    ]),
                }
            ))
        );
    }
}
