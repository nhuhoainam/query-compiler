use std::fmt;

use debug_tree::TreeBuilder;
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
    arithmetic::arithmetic_expression,
    column::Column,
    common::{
        column_identifier, field_list, literal_expression, statement_terminator, table_list,
        table_reference, ws_sep_comma, FieldDefinitionExpression, FieldValueExpression, TreeNode,
    },
    condition::{condition_expr, ConditionExpression},
    join::{join_clause, JoinClause},
    order::{order_by_clause, OrderByClause},
    table::Table,
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
    fn populate(&self, parent: &TreeBuilder) {
        let mut branch = parent
            .add_branch(format!("SELECT{}", if self.distinct { " DISTINCT" } else { "" }).as_str());
        for sel_item in self.sel_list.iter() {
            sel_item.populate(parent);
        }
        branch.release();
        let mut branch = parent.add_branch("FROM");
        for table in self.tables.iter() {
            table.populate(parent);
        }
        for join in self.join.iter() {
            join.populate(parent);
        }
        branch.release();
        if let Some(ref condition) = self.condition {
            let mut branch = parent.add_branch("WHERE");
            condition.populate(parent);
            branch.release();
        }
        if let Some(ref group_by) = self.group_by {
            let mut branch = parent.add_branch("GROUP BY");
            for col in group_by.columns.iter() {
                col.populate(parent);
            }
            branch.release();
            if let Some(ref having) = group_by.having {
                let mut branch = parent.add_branch("HAVING");
                having.populate(parent);
                branch.release();
            }
        }
        if let Some(ref order_by) = self.order_by {
            let mut branch = parent.add_branch("ORDER BY");
            for col in order_by.columns.iter() {
                add_leaf!("{} {}", col.0.name, col.1);
            }
            branch.release();
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
            map(literal_expression, |lit| {
                FieldDefinitionExpression::FieldValue(FieldValueExpression::Literal(lit))
            }),
            map(column_identifier, |col| {
                FieldDefinitionExpression::Column(col)
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
