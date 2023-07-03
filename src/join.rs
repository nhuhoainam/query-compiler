use nom::{IResult, combinator::{map, opt}, sequence::{tuple, delimited, terminated, preceded}, bytes::complete::{tag_no_case, tag}, character::complete::{multispace1, multispace0}, branch::alt};

use crate::{
    column::Column,
    condition::{ConditionExpression, condition_expr},
    select::{SelectStatement, nested_select_statement},
    table::Table, common::{field_list, as_alias, table_reference, table_list},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct JoinClause {
    pub operator: JoinOperator,
    pub right: JoinRightHand,
    pub constraint: JoinCondition,
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
pub enum JoinCondition {
    On(ConditionExpression),
    Using(Vec<Column>),
}

// Parse binary comparison operators
pub fn join_operator(i: &[u8]) -> IResult<&[u8], JoinOperator> {
    alt((
        map(tag_no_case("join"), |_| JoinOperator::Natural),
        map(tag_no_case("natural join"), |_| JoinOperator::Natural),
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
            multispace1,
            delimited(
                terminated(tag("("), multispace0),
                field_list,
                preceded(multispace0, tag(")")),
            ),
        )),
        |t| JoinCondition::Using(t.2),
    );
    let on_constraint = alt((
        delimited(
            terminated(tag("("), multispace0),
            condition_expr,
            preceded(multispace0, tag(")")),
        ),
        condition_expr,
    ));
    let on_clause = map(tuple((tag_no_case("on"), multispace1, on_constraint)), |t| {
        JoinCondition::On(t.2)
    });

    alt((using_clause, on_clause))(i)
}

// Parse JOIN clause
fn join_clause(i: &[u8]) -> IResult<&[u8], JoinClause> {
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
