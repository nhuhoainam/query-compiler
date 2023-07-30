use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{multispace0, multispace1},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, terminated, tuple},
    IResult,
};

use crate::{
    column::Column,
    common::{
        column_identifier, field_list, literal_expression, statement_terminator, table_list,
        table_reference, ws_sep_comma, FieldDefinitionExpression, FieldValueExpression, TreeNode,
    },
    condition::{condition_expr, ConditionExpression},
    join::{join_clause, JoinClause},
    order::{order_by_clause, OrderByClause},
    table::Table, arithmetic::arithmetic_expression,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum FromItem {
    Table(Table),
    NestedSelect(SelectStatement),
    Join(JoinClause),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct GroupByClause {
    pub columns: Vec<Column>,
    pub having: Option<ConditionExpression>,
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct SelectStatement {
    pub distinct: bool,
    pub sel_list: Vec<FieldDefinitionExpression>,
    pub tables: Vec<Table>,
    pub join: Vec<JoinClause>,
    pub condition: Option<ConditionExpression>,
    pub group_by: Option<GroupByClause>,
    pub order_by: Option<OrderByClause>,
}

impl TreeNode for SelectStatement {
    fn populate(&self) {
        add_branch!("<Query>");
        add_branch!("SELECT{}", if self.distinct { " DISTINCT" } else { "" });
        for sel_item in self.sel_list.iter() {
            sel_item.populate();
        }
        add_branch!("FROM");
        for table in self.tables.iter() {
            table.populate();
        }
        for join in self.join.iter() {
            join.populate();
        }
        if let Some(ref condition) = self.condition {
            add_branch!("WHERE");
            condition.populate();
        }
        if let Some(ref group_by) = self.group_by {
            add_branch!("GROUP BY");
            for col in group_by.columns.iter() {
                col.populate();
            }
            if let Some(ref having) = group_by.having {
                add_branch!("HAVING");
                having.populate();
            }
        }
        if let Some(ref order_by) = self.order_by {
            add_branch!("ORDER BY");
            for col in order_by.columns.iter() {
                add_leaf!("{} {}", col.0.name, col.1);
            }
        }
    }
}

impl fmt::Display for SelectStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

fn having_clause(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let (remaining_input, (_, _, _, ce)) = tuple((
        multispace0,
        tag_no_case("having"),
        multispace1,
        condition_expr,
    ))(i)?;

    Ok((remaining_input, ce))
}

/// Parse GROUP BY clause
pub fn group_by_clause(i: &[u8]) -> IResult<&[u8], GroupByClause> {
    let (remaining_input, (_, _, _, columns, having)) = tuple((
        multispace0,
        tag_no_case("group by"),
        multispace1,
        field_list,
        opt(having_clause),
    ))(i)?;

    Ok((remaining_input, GroupByClause { columns, having }))
}

pub fn field_definition_expression(
    i: &[u8],
) -> nom::IResult<&[u8], Vec<FieldDefinitionExpression>> {
    many0(terminated(
        alt((
            map(tag("*"), |_| FieldDefinitionExpression::All),
            map(terminated(table_reference, tag(".*")), |table| {
                FieldDefinitionExpression::AllFromTable(table)
            }),
            map(arithmetic_expression, |expr| {
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Arithmetic(expr))
            }),
            map(column_identifier, |col| {
                FieldDefinitionExpression::Column(col)
            }),
            map(literal_expression, |lit| {
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(lit))
            }),
        )),
        opt(ws_sep_comma),
    ))(i)
}

/// Parse WHERE clause of a selection
pub fn where_clause(i: &[u8]) -> IResult<&[u8], ConditionExpression> {
    let (remaining_input, (_, _, _, where_condition)) = tuple((
        multispace0,
        tag_no_case("where"),
        multispace1,
        condition_expr,
    ))(i)?;

    Ok((remaining_input, where_condition))
}

pub fn select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    terminated(nested_select_statement, statement_terminator)(i)
}

pub fn nested_select_statement(i: &[u8]) -> nom::IResult<&[u8], SelectStatement> {
    let (
        remaining_input,
        (_, _, distinct, _, sel_list, _, tables, join, condition, group_by, order_by),
    ) = tuple((
        tag_no_case("select"),
        multispace1,
        opt(tag_no_case("distinct")),
        multispace0,
        field_definition_expression,
        delimited(multispace0, tag_no_case("from"), multispace0),
        table_list,
        many0(join_clause),
        opt(where_clause),
        opt(group_by_clause),
        opt(order_by_clause),
    ))(i)?;

    Ok((
        remaining_input,
        SelectStatement {
            distinct: distinct.is_some(),
            sel_list,
            tables,
            join,
            condition,
            group_by,
            order_by,
        },
    ))
}