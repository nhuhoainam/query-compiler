use std::fmt;

use nom::{IResult, branch::alt, combinator::{map, opt}, sequence::{preceded, delimited, tuple}, bytes::complete::{tag_no_case, tag}, character::complete::{multispace1, multispace0}, multi::{many1, many0}};

use crate::{select::{SelectStatement, nested_select_statement}, order::{OrderByClause, order_by_clause}, common::{opt_delimited, statement_terminator, TreeNode}};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub enum CompoundSelectOperator {
    Union,
    DistinctUnion,
    Intersect,
    Except,
}

impl fmt::Display for CompoundSelectOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CompoundSelectOperator::Union => write!(f, "UNION"),
            CompoundSelectOperator::DistinctUnion => write!(f, "UNION DISTINCT"),
            CompoundSelectOperator::Intersect => write!(f, "INTERSECT"),
            CompoundSelectOperator::Except => write!(f, "EXCEPT"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub struct CompoundSelectStatement {
    pub selects: Vec<(Option<CompoundSelectOperator>, SelectStatement)>,
    pub order: Option<OrderByClause>,
}

impl TreeNode for CompoundSelectStatement {
    fn populate(&self, parent: &debug_tree::TreeBuilder) {
        let mut branch = parent.add_branch("<Query>");
        for (ref op, ref sel) in &self.selects {
            if op.is_some() {
                let mut branch = parent.add_branch(format!("{}", op.as_ref().unwrap()).as_str());
                branch.release();
            }
            sel.populate(parent);
        }
        if let Some(ref order_by) = self.order {
            let mut branch = parent.add_branch("ORDER BY");
            for col in order_by.columns.iter() {
                add_leaf!("{} {}", col.0.name, col.1);
            }
            branch.release();
        }
        branch.release();
    }
}

impl fmt::Display for CompoundSelectStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (ref op, ref sel) in &self.selects {
            if op.is_some() {
                write!(f, " {}", op.as_ref().unwrap())?;
            }
            write!(f, " {}", sel)?;
        }
        if self.order.is_some() {
            write!(f, " {}", self.order.as_ref().unwrap())?;
        }
        Ok(())
    }
}

// Parse compound operator
fn compound_op(i: &[u8]) -> IResult<&[u8], CompoundSelectOperator> {
    alt((
        map(
            preceded(
                tag_no_case("union"),
                opt(preceded(
                    multispace1,
                    alt((
                        map(tag_no_case("all"), |_| false),
                        map(tag_no_case("distinct"), |_| true),
                    )),
                )),
            ),
            |distinct| match distinct {
                // DISTINCT is the default in both MySQL and SQLite
                None => CompoundSelectOperator::DistinctUnion,
                Some(d) => {
                    if d {
                        CompoundSelectOperator::DistinctUnion
                    } else {
                        CompoundSelectOperator::Union
                    }
                }
            },
        ),
        map(tag_no_case("intersect"), |_| {
            CompoundSelectOperator::Intersect
        }),
        map(tag_no_case("except"), |_| CompoundSelectOperator::Except),
    ))(i)
}

fn other_selects(i: &[u8]) -> IResult<&[u8], (Option<CompoundSelectOperator>, SelectStatement)> {
    let (remaining_input, (_, op, _, select)) = tuple((
        multispace0,
        compound_op,
        multispace1,
        opt_delimited(
            tag("("),
            delimited(multispace0, nested_select_statement, multispace0),
            tag(")"),
        ),
    ))(i)?;

    Ok((remaining_input, (Some(op), select)))
}

// Parse compound selection
pub fn compound_selection(i: &[u8]) -> IResult<&[u8], CompoundSelectStatement> {
    let (remaining_input, (first_select, other_selects, _, order, _)) = tuple((
        opt_delimited(tag("("), nested_select_statement, tag(")")),
        many0(other_selects),
        multispace0,
        opt(order_by_clause),
        statement_terminator,
    ))(i)?;

    let mut selects = vec![(None, first_select)];
    selects.extend(other_selects);

    Ok((
        remaining_input,
        CompoundSelectStatement {
            selects,
            order,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::column::Column;
    use crate::common::{FieldDefinitionExpression, FieldValueExpression, Literal};
    use crate::table::Table;

    #[test]
    fn union() {
        let qstr = "SELECT id, 1 FROM Vote UNION SELECT id, stars from Rating;";
        let qstr2 = "(SELECT id, 1 FROM Vote) UNION (SELECT id, stars from Rating);";
        let res = compound_selection(qstr.as_bytes());
        let res2 = compound_selection(qstr2.as_bytes());

        let first_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            sel_list: vec![
                FieldDefinitionExpression::Column(Column::from("id")),
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(
                    Literal::Integer(1).into(),
                )),
            ],
            ..Default::default()
        };
        let second_select = SelectStatement {
            tables: vec![Table::from("Rating")],
            sel_list: vec![
                FieldDefinitionExpression::Column(Column::from("id")),
                FieldDefinitionExpression::Column(Column::from("stars")),
            ],
            ..Default::default()
        };
        let expected = CompoundSelectStatement {
            selects: vec![
                (None, first_select),
                (Some(CompoundSelectOperator::DistinctUnion), second_select),
            ],
            order: None,
        };

        assert_eq!(res.unwrap().1, expected);
        assert_eq!(res2.unwrap().1, expected);
    }

    #[test]
    fn union_strict() {
        let qstr = "SELECT id, 1 FROM Vote);";
        let qstr2 = "(SELECT id, 1 FROM Vote;";
        let qstr3 = "SELECT id, 1 FROM Vote) UNION (SELECT id, stars from Rating;";
        let res = compound_selection(qstr.as_bytes());
        let res2 = compound_selection(qstr2.as_bytes());
        let res3 = compound_selection(qstr3.as_bytes());

        assert!(&res.is_err());
        assert_eq!(
            res.unwrap_err(),
            nom::Err::Error(nom::error::Error::new(");".as_bytes(), nom::error::ErrorKind::Tag))
        );
        assert!(&res2.is_err());
        assert_eq!(
            res2.unwrap_err(),
            nom::Err::Error(nom::error::Error::new(";".as_bytes(), nom::error::ErrorKind::Tag))
        );
        assert!(&res3.is_err());
        assert_eq!(
            res3.unwrap_err(),
            nom::Err::Error(nom::error::Error::new(
                ") UNION (SELECT id, stars from Rating;".as_bytes(),
                nom::error::ErrorKind::Tag
            ))
        );
    }

    #[test]
    fn multi_union() {
        let qstr = "SELECT id, 1 FROM Vote \
                    UNION SELECT id, stars from Rating \
                    UNION DISTINCT SELECT 42, 5 FROM Vote;";
        let res = compound_selection(qstr.as_bytes());

        let first_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            sel_list: vec![
                FieldDefinitionExpression::Column(Column::from("id")),
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(
                    Literal::Integer(1).into(),
                )),
            ],
            ..Default::default()
        };
        let second_select = SelectStatement {
            tables: vec![Table::from("Rating")],
            sel_list: vec![
                FieldDefinitionExpression::Column(Column::from("id")),
                FieldDefinitionExpression::Column(Column::from("stars")),
            ],
            ..Default::default()
        };
        let third_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            sel_list: vec![
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(
                    Literal::Integer(42).into(),
                )),
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(
                    Literal::Integer(5).into(),
                )),
            ],
            ..Default::default()
        };

        let expected = CompoundSelectStatement {
            selects: vec![
                (None, first_select),
                (Some(CompoundSelectOperator::DistinctUnion), second_select),
                (Some(CompoundSelectOperator::DistinctUnion), third_select),
            ],
            order: None,
        };

        assert_eq!(res.unwrap().1, expected);
    }

    #[test]
    fn union_all() {
        let qstr = "SELECT id, 1 FROM Vote UNION ALL SELECT id, stars from Rating;";
        let res = compound_selection(qstr.as_bytes());

        let first_select = SelectStatement {
            tables: vec![Table::from("Vote")],
            sel_list: vec![
                FieldDefinitionExpression::Column(Column::from("id")),
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(
                    Literal::Integer(1).into(),
                )),
            ],
            ..Default::default()
        };
        let second_select = SelectStatement {
            tables: vec![Table::from("Rating")],
            sel_list: vec![
                FieldDefinitionExpression::Column(Column::from("id")),
                FieldDefinitionExpression::Column(Column::from("stars")),
            ],
            ..Default::default()
        };
        let expected = CompoundSelectStatement {
            selects: vec![
                (None, first_select),
                (Some(CompoundSelectOperator::Union), second_select),
            ],
            order: None,
        };

        assert_eq!(res.unwrap().1, expected);
    }
}
