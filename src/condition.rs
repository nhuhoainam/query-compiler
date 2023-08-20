use std::fmt;

use debug_tree::TreeBuilder;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{multispace0, multispace1},
    combinator::{map, opt},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    arithmetic::{arithmetic_expression, ArithmeticExpression},
    column::Column,
    common::{
        binary_comparison_operator, column_identifier, literal, value_list, Literal, Operator, TreeNode,
    },
    select::{nested_select_statement, SelectStatement},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ConditionBase {
    Field(Column),
    Literal(Literal),
    LiteralList(Vec<Literal>),
    NestedSelect(Box<SelectStatement>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct ConditionTree {
    pub operator: Operator,
    pub left: Box<ConditionExpression>,
    pub right: Box<ConditionExpression>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum ConditionExpression {
    ComparisonOp(ConditionTree),
    LogicalOp(ConditionTree),
    NegationOp(Box<ConditionExpression>),
    ExistsOp(Box<SelectStatement>),
    Base(ConditionBase),
    Arithmetic(Box<ArithmeticExpression>),
    Bracketed(Box<ConditionExpression>),
}

impl TreeNode for ConditionExpression {
    fn populate(&self, parent: &TreeBuilder) {
        match self {
            ConditionExpression::ComparisonOp(c) => {
                let mut branch = parent.add_branch(format!("{}", c.operator).as_str());
                c.left.populate(parent);
                c.right.populate(parent);
                branch.release();
            }
            ConditionExpression::LogicalOp(c) => {
                let mut branch = parent.add_branch(format!("{}", c.operator).as_str());
                c.left.populate(parent);
                c.right.populate(parent);
                branch.release();
            }
            ConditionExpression::NegationOp(c) => {
                let mut branch = parent.add_branch("NOT");
                c.populate(parent);
                branch.release();
            }
            ConditionExpression::ExistsOp(s) => {
                let mut branch = parent.add_branch("EXISTS");
                s.populate(parent);
                branch.release();
            }
            ConditionExpression::Base(b) => b.populate(parent),
            ConditionExpression::Arithmetic(a) => a.populate(parent),
            ConditionExpression::Bracketed(c) => {
                let mut branch = parent.add_branch("()");
                c.populate(parent);
                branch.release();
            }
        }
    }
}

impl TreeNode for ConditionBase {
    fn populate(&self, parent: &TreeBuilder) {
        match self {
            ConditionBase::Field(c) => c.populate(parent),
            ConditionBase::Literal(l) => parent.add_leaf(format!("{}", l).as_str()),
            ConditionBase::LiteralList(l) => {
                let mut branch = parent.add_branch("()");
                for lit in l.iter() {
                    parent.add_leaf(format!("{}", lit).as_str());
                }
                branch.release();
            }
            ConditionBase::NestedSelect(s) => s.populate(parent),
        }
    }
}

impl fmt::Display for ConditionBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConditionBase::Field(c) => write!(f, "{}", c),
            ConditionBase::Literal(l) => write!(f, "{}", l),
            ConditionBase::LiteralList(l) => {
                write!(
                    f,
                    "({})",
                    l.iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            ConditionBase::NestedSelect(s) => write!(f, "({})", s),
        }
    }
}

impl fmt::Display for ConditionTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

impl fmt::Display for ConditionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConditionExpression::ComparisonOp(c) => write!(f, "{}", c),
            ConditionExpression::LogicalOp(c) => write!(f, "{}", c),
            ConditionExpression::NegationOp(c) => write!(f, "NOT {}", c),
            ConditionExpression::ExistsOp(s) => write!(f, "EXISTS {}", s),
            ConditionExpression::Base(b) => write!(f, "{}", b),
            ConditionExpression::Arithmetic(a) => write!(f, "{}", a),
            ConditionExpression::Bracketed(c) => write!(f, "({})", c),
        }
    }
}

fn predicate(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let nested_exist = map(
        tuple((
            opt(delimited(multispace0, tag_no_case("not"), multispace1)),
            delimited(multispace0, tag_no_case("exist"), multispace0),
            delimited(multispace0, nested_select_statement, multispace0),
        )),
        |p| {
            let nested = ConditionExpression::ExistsOp(Box::new(p.2));
            match p.0 {
                Some(_) => ConditionExpression::NegationOp(Box::new(nested)),
                None => nested,
            }
        },
    );

    alt((simple_expr, nested_exist))(i)
}

pub fn not_expr(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    alt((
        map(
            preceded(pair(tag_no_case("not"), multispace1), parenthesized_expr),
            |right| ConditionExpression::NegationOp(Box::new(right)),
        ),
        boolean_primary,
    ))(i)
}

pub fn and_expr(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let cond = map(
        separated_pair(
            parenthesized_expr,
            delimited(multispace0, tag_no_case("and"), multispace1),
            and_expr,
        ),
        |p| {
            ConditionExpression::LogicalOp(ConditionTree {
                operator: Operator::And,
                left: Box::new(p.0),
                right: Box::new(p.1),
            })
        },
    );

    alt((cond, parenthesized_expr))(i)
}

fn is_null(i: &[u8]) -> IResult<&[u8], (Operator, ConditionExpression)> {
    let (remaining_input, (_, _, not, _, _)) = tuple((
        tag_no_case("is"),
        multispace0,
        opt(tag_no_case("not")),
        multispace0,
        tag_no_case("null"),
    ))(i)?;

    Ok((
        remaining_input,
        (
            match not {
                Some(_) => Operator::NotEqual,
                None => Operator::Equal,
            },
            ConditionExpression::Base(ConditionBase::Literal(Literal::Null)),
        ),
    ))
}

fn in_operation(i: &[u8]) -> IResult<&[u8], (Operator, ConditionExpression)> {
    map(
        separated_pair(
            opt(terminated(tag_no_case("not"), multispace1)),
            terminated(tag_no_case("in"), multispace0),
            alt((
                map(
                    delimited(
                        terminated(tag("("), multispace0), 
                        nested_select_statement,
                        preceded(multispace0, tag(")"))
                    ),
                    |s| ConditionBase::NestedSelect(Box::new(s)),
                ),
                map(delimited(
                    terminated(tag("("), multispace0), 
                    value_list, 
                    preceded(multispace0, tag(")"))
                ), |vs| {
                    ConditionBase::LiteralList(vs)
                }),
            )),
        ),
        |p| {
            let nested = ConditionExpression::Base(p.1);
            (
                match p.0 {
                    Some(_) => Operator::NotIn,
                    None => Operator::In,
                },
                nested,
            )
        },
    )(i)
}

fn boolean_primary_rest(i: &[u8]) -> IResult<&[u8], (Operator, ConditionExpression)> {
    alt((
        is_null,
        in_operation,
        separated_pair(binary_comparison_operator, multispace0, predicate),
    ))(i)
}

fn boolean_primary(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    alt((
        map(
            separated_pair(predicate, multispace0, boolean_primary_rest),
            |e: (ConditionExpression, (Operator, ConditionExpression))| {
                ConditionExpression::ComparisonOp(ConditionTree {
                    operator: (e.1).0,
                    left: Box::new(e.0),
                    right: Box::new((e.1).1),
                })
            },
        ),
        predicate,
    ))(i)
}

/// Parse a conditional expression into a condition tree structure
pub fn condition_expr(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let cond = map(
        separated_pair(
            and_expr,
            delimited(multispace0, tag_no_case("or"), multispace1),
            condition_expr,
        ),
        |p| {
            ConditionExpression::LogicalOp(ConditionTree {
                operator: Operator::Or,
                left: Box::new(p.0),
                right: Box::new(p.1),
            })
        },
    );

    alt((cond, and_expr))(i)
}

fn simple_expr(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    alt((
        map(
            delimited(
                terminated(tag("("), multispace0),
                arithmetic_expression,
                preceded(multispace0, tag(")")),
            ),
            |e| {
                ConditionExpression::Bracketed(Box::new(ConditionExpression::Arithmetic(Box::new(
                    e,
                ))))
            },
        ),
        map(arithmetic_expression, |e| {
            ConditionExpression::Arithmetic(Box::new(e))
        }),
        map(literal, |lit| {
            ConditionExpression::Base(ConditionBase::Literal(lit))
        }),
        map(column_identifier, |f| {
            ConditionExpression::Base(ConditionBase::Field(f))
        }),
        map(
            delimited(
                terminated(tag("("), multispace0), 
                nested_select_statement, 
                preceded(multispace0, tag(")")),
            ),
            |s| ConditionExpression::Base(ConditionBase::NestedSelect(Box::new(s))),
        ),
    ))(i)
}

fn parenthesized_expr_helper(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let (remaining_input, (_, _, left_expr, _, _, _, operator, _, right_expr)) = tuple((
        tag("("),
        multispace0,
        simple_expr,
        multispace0,
        tag(")"),
        multispace0,
        binary_comparison_operator,
        multispace0,
        simple_expr,
    ))(i)?;

    let left = Box::new(ConditionExpression::Bracketed(Box::new(left_expr)));
    let right = Box::new(right_expr);
    let condition = ConditionExpression::ComparisonOp(ConditionTree {
        operator,
        left,
        right,
    });

    Ok((remaining_input, condition))
}

pub fn parenthesized_expr(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    alt((
        parenthesized_expr_helper,
        map(
            delimited(
                terminated(tag("("), multispace0),
                condition_expr,
                delimited(multispace0, tag(")"), multispace0),
            ),
            |inner| ConditionExpression::Bracketed(Box::new(inner)),
        ),
        not_expr,
    ))(i)
}

#[cfg(test)]
mod tests {
    use crate::{common::FieldDefinitionExpression, table::{Table, self}, select, column::FunctionExpression};

    use super::*;

    fn columns(cols: &[&str]) -> Vec<FieldDefinitionExpression> {
        cols.iter()
            .map(|c| FieldDefinitionExpression::Column(Column::from(*c)))
            .collect()
    }

    fn flat_condition_tree(
        op: Operator,
        l: ConditionBase,
        r: ConditionBase,
    ) -> ConditionExpression {
        ConditionExpression::ComparisonOp(ConditionTree {
            operator: op,
            left: Box::new(ConditionExpression::Base(l)),
            right: Box::new(ConditionExpression::Base(r)),
        })
    }

    #[test]
    fn test_condition_expr() {
        let a_cond = ConditionExpression::ComparisonOp(ConditionTree {
            operator: Operator::Equal,
            left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                Column::from("a"),
            ))),
            right: Box::new(ConditionExpression::Base(ConditionBase::Literal(
                Literal::Integer(1),
            ))),
        });
        let b_cond = ConditionExpression::ComparisonOp(ConditionTree {
            operator: Operator::Equal,
            left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                Column::from("b"),
            ))),
            right: Box::new(ConditionExpression::Base(ConditionBase::Literal(
                Literal::Integer(2),
            ))),
        });
        let c_cond = ConditionExpression::ComparisonOp(ConditionTree {
            operator: Operator::Equal,
            left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                Column::from("c"),
            ))),
            right: Box::new(ConditionExpression::Base(ConditionBase::Literal(
                Literal::Integer(3),
            ))),
        });
        let d_cond = ConditionExpression::ComparisonOp(ConditionTree {
            operator: Operator::Equal,
            left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                Column::from("d"),
            ))),
            right: Box::new(ConditionExpression::Base(ConditionBase::Literal(
                Literal::Integer(4),
            ))),
        });

        assert_eq!(condition_expr(b"a = 1"), Ok((&b""[..], a_cond.clone())));
        assert_eq!(
            condition_expr(b"a = 1 and b = 2"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::And,
                    left: Box::new(a_cond.clone()),
                    right: Box::new(b_cond.clone()),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"a = 1 or b = 2"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::Or,
                    left: Box::new(a_cond.clone()),
                    right: Box::new(b_cond.clone()),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"(a = 1) or b = 2"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::Or,
                    left: Box::new(ConditionExpression::Bracketed(Box::new(a_cond.clone()))),
                    right: Box::new(b_cond.clone()),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"a = 1 or (b = 2)"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::Or,
                    left: Box::new(a_cond.clone()),
                    right: Box::new(ConditionExpression::Bracketed(Box::new(b_cond.clone()))),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"(a = 1) or (b = 2)"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::Or,
                    left: Box::new(ConditionExpression::Bracketed(Box::new(a_cond.clone()))),
                    right: Box::new(ConditionExpression::Bracketed(Box::new(b_cond.clone()))),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"(a = 1 or b = 2)"),
            Ok((
                &b""[..],
                ConditionExpression::Bracketed(Box::new(ConditionExpression::LogicalOp(
                    ConditionTree {
                        operator: Operator::Or,
                        left: Box::new(a_cond.clone()),
                        right: Box::new(b_cond.clone()),
                    }
                )))
            ))
        );
        assert_eq!(
            condition_expr(b"((a = 1) or (b = 2))"),
            Ok((
                &b""[..],
                ConditionExpression::Bracketed(Box::new(ConditionExpression::LogicalOp(
                    ConditionTree {
                        operator: Operator::Or,
                        left: Box::new(ConditionExpression::Bracketed(Box::new(a_cond.clone()))),
                        right: Box::new(ConditionExpression::Bracketed(Box::new(b_cond.clone()))),
                    }
                )))
            ))
        );
        assert_eq!(
            condition_expr(b"((a = 1) or (b = 2)) and c = 3"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::And,
                    left: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(ConditionExpression::Bracketed(Box::new(
                                a_cond.clone()
                            ))),
                            right: Box::new(ConditionExpression::Bracketed(Box::new(
                                b_cond.clone()
                            ))),
                        })
                    ))),
                    right: Box::new(c_cond.clone()),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"((a = 1) or (b = 2)) and (c = 3)"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::And,
                    left: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(ConditionExpression::Bracketed(Box::new(
                                a_cond.clone()
                            ))),
                            right: Box::new(ConditionExpression::Bracketed(Box::new(
                                b_cond.clone()
                            ))),
                        })
                    ))),
                    right: Box::new(ConditionExpression::Bracketed(Box::new(c_cond.clone()))),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"((a = 1) or (b = 2)) and (c = 3 or d = 4)"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::And,
                    left: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(ConditionExpression::Bracketed(Box::new(
                                a_cond.clone()
                            ))),
                            right: Box::new(ConditionExpression::Bracketed(Box::new(
                                b_cond.clone()
                            ))),
                        })
                    ))),
                    right: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(c_cond.clone()),
                            right: Box::new(d_cond.clone()),
                        })
                    ))),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"((a = 1) or (b = 2)) and (c = 3 or (d = 4))"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::And,
                    left: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(ConditionExpression::Bracketed(Box::new(
                                a_cond.clone()
                            ))),
                            right: Box::new(ConditionExpression::Bracketed(Box::new(
                                b_cond.clone()
                            ))),
                        })
                    ))),
                    right: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(c_cond.clone()),
                            right: Box::new(ConditionExpression::Bracketed(Box::new(
                                d_cond.clone()
                            ))),
                        })
                    ))),
                })
            ))
        );
        assert_eq!(
            condition_expr(b"((a = 1) or (b = 2)) and ((c = 3) or (d = 4))"),
            Ok((
                &b""[..],
                ConditionExpression::LogicalOp(ConditionTree {
                    operator: Operator::And,
                    left: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(ConditionExpression::Bracketed(Box::new(
                                a_cond.clone()
                            ))),
                            right: Box::new(ConditionExpression::Bracketed(Box::new(
                                b_cond.clone()
                            ))),
                        })
                    ))),
                    right: Box::new(ConditionExpression::Bracketed(Box::new(
                        ConditionExpression::LogicalOp(ConditionTree {
                            operator: Operator::Or,
                            left: Box::new(ConditionExpression::Bracketed(Box::new(
                                c_cond.clone()
                            ))),
                            right: Box::new(ConditionExpression::Bracketed(Box::new(
                                d_cond.clone()
                            ))),
                        })
                    ))),
                })
            ))
        );
    }

    #[test]
    fn comparison_nested_select() {
        let cond = "salary = ALL ( SELECT MAX(salary) FROM Instructor )";

        let res = condition_expr(cond.as_bytes());

        let nested_select = Box::new(SelectStatement {
            sel_list: vec![
                FieldDefinitionExpression::Column(Column { 
                    name: "salary".to_string(), 
                    alias: None, 
                    table: None, 
                    function: Some(Box::new(FunctionExpression::Max(Column::from("salary")))),
                }),
            ],
            tables: vec![Table::from("Instructor")],
            ..Default::default()
        });

        let expected = ConditionExpression::ComparisonOp(ConditionTree {
            operator: Operator::All(Box::new(Operator::Equal)),
            left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                Column::from("salary"),
            ))),
            right: Box::new(ConditionExpression::Base(ConditionBase::NestedSelect(
                nested_select,
            ))),
        });

        assert_eq!(res.unwrap().1, expected);
    }

    #[test]
    fn nested_select() {
        let cond = "ID NOT IN ( SELECT DISTINCT ID FROM Takes WHERE Takes.[year] < 2023 ) ";

        let res = condition_expr(cond.as_bytes());

        let nested_select = Box::new(SelectStatement {
            distinct: true,
            sel_list: columns(&["ID"]),
            tables: vec![Table::from("Takes")],
            condition: Some(ConditionExpression::ComparisonOp(ConditionTree {
                operator: Operator::Less,
                left: Box::new(ConditionExpression::Base(ConditionBase::Field(
                    Column::from("Takes.year"),
                ))),
                right: Box::new(ConditionExpression::Base(ConditionBase::Literal(
                    Literal::Integer(2023),
                ))),
            })),
            ..Default::default()
        });

        let expected = ConditionExpression::ComparisonOp(ConditionTree {
            operator: Operator::NotIn,
            left: Box::new(ConditionExpression::Base(ConditionBase::Field(Column::from("ID")))),
            right: Box::new(ConditionExpression::Base(ConditionBase::NestedSelect(
                nested_select,
            ))),
        });

        assert_eq!(res.unwrap().1, expected);
    }

    #[test]
    fn and_with_nested_select() {
        use select::SelectStatement;
        use std::default::Default;
        use table::Table;
        use ConditionBase::*;

        let cond = "paperId not in (select paperId from PaperConflict) and size > 0";

        let res = condition_expr(cond.as_bytes());

        let nested_select = Box::new(SelectStatement {
            tables: vec![Table::from("PaperConflict")],
            sel_list: columns(&["paperId"]),
            ..Default::default()
        });

        let left = flat_condition_tree(
            Operator::NotIn,
            Field("paperId".into()),
            NestedSelect(nested_select),
        );

        let right = flat_condition_tree(Operator::Greater, Field("size".into()), Literal(0.into()));

        let expected = ConditionExpression::LogicalOp(ConditionTree {
            left: Box::new(left),
            right: Box::new(right),
            operator: Operator::And,
        });

        assert_eq!(res.unwrap().1, expected);
    }

    #[test]
    fn not_in_comparison() {
        use ConditionBase::*;

        let qs1 = b"id not in (1,2)";
        let res1 = condition_expr(qs1);

        let c1 = res1.unwrap().1;
        let expected1 = flat_condition_tree(
            Operator::NotIn,
            Field("id".into()),
            LiteralList(vec![1.into(), 2.into()]),
        );
        assert_eq!(c1, expected1);

        let expected1 = "id NOT IN (1, 2)";
        assert_eq!(format!("{}", c1), expected1);
    }
}
