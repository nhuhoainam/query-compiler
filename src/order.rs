use std::fmt;

use nom::{branch::alt, combinator::{map, opt}, bytes::complete::tag_no_case, IResult, sequence::{tuple, preceded}, character::complete::{multispace0, multispace1}, multi::many0};

use crate::{column::Column, keywords::escape_if_keyword, common::{column_identifier_no_alias, ws_sep_comma}};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum OrderType {
    Ascending,
    Descending,
}

impl fmt::Display for OrderType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OrderType::Ascending => write!(f, "ASC"),
            OrderType::Descending => write!(f, "DESC"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OrderByClause {
    pub columns: Vec<(Column, OrderType)>,
}

impl fmt::Display for OrderByClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ORDER BY ")?;
        write!(
            f,
            "{}",
            self.columns
                .iter()
                .map(|&(ref c, ref o)| format!("{} {}", escape_if_keyword(&c.name), o))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

pub fn order_type(i: &[u8]) -> nom::IResult<&[u8], OrderType> {
    alt((
        map(tag_no_case("asc"), |_| OrderType::Ascending),
        map(tag_no_case("desc"), |_| OrderType::Descending),
    ))(i)
}

fn order_expr(i: &[u8]) -> IResult<&[u8], (Column, OrderType)> {
    let (remaining_input, (field_name, ordering, _)) = tuple((
        column_identifier_no_alias,
        opt(preceded(multispace0, order_type)),
        opt(ws_sep_comma),
    ))(i)?;

    Ok((
        remaining_input,
        (field_name, ordering.unwrap_or(OrderType::Ascending)),
    ))
}

// Parse ORDER BY clause
pub fn order_by_clause(i: &[u8]) -> IResult<&[u8], OrderByClause> {
    let (remaining_input, (_, _, _, columns)) = tuple((
        multispace0,
        tag_no_case("order by"),
        multispace1,
        many0(order_expr),
    ))(i)?;

    Ok((remaining_input, OrderByClause { columns }))
}