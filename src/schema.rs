use std::{collections::BTreeMap, str::from_utf8};

use nom::{
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, terminated, tuple},
    IResult,
};

use crate::{
    column::Column,
    common::{sql_identifier, ws_sep_comma},
};

pub type Schema = BTreeMap<String, Vec<Column>>;

pub fn print_schema(schema: Schema) {
    for (table_name, columns) in schema.iter() {
        println!("{}:", table_name);
        for column in columns.iter() {
            println!("\t{}", column.name);
        }
    }
}

pub fn schema(i: &[u8]) -> IResult<&[u8], Schema> {
    map(many0(terminated(relation, multispace0)), |relations| {
        relations.into_iter().collect()
    })(i)
}

pub fn relation(i: &[u8]) -> IResult<&[u8], (String, Vec<Column>)> {
    map(
        tuple((
            sql_identifier,
            multispace0,
            delimited(tag("("), columns_definition, tag(")")),
        )),
        |(table_name, _, cols)| (from_utf8(table_name).unwrap().to_string(), cols),
    )(i)
}

pub fn columns_definition(i: &[u8]) -> IResult<&[u8], Vec<Column>> {
    many0(terminated(
        map(sql_identifier, |col_name| Column {
            name: from_utf8(col_name).unwrap().to_string(),
            alias: None,
            table: None,
            function: None,
        }),
        opt(ws_sep_comma),
    ))(i)
}
