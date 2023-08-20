use std::{collections::BTreeMap, fs};

use debug_tree::TreeBuilder;
use query_compiler::{
    column::Column, common::TreeNode, logical_plan::Relation, schema::{Schema, schema, print_schema},
    select::select_statement,
};

fn main() {
    run_schema();
    // run_query();
}

fn run_schema() {
    let input = fs::read_to_string("schema.txt").unwrap();
    let res = schema(input.as_bytes()).unwrap().1;
    print_schema(res);
}

fn run_query() {
    let input = fs::read_to_string("input.txt").unwrap();
    let parse_res = select_statement(input.as_bytes());
    let parse_tree = TreeBuilder::new();
    let parse = parse_res.unwrap().1;
    parse.populate(&parse_tree);
    parse_tree.write("output_parse_tree.txt").ok();
    let schema = get_schema();
    let res: Relation = (parse, schema).into();
    let ra_tree = TreeBuilder::new();
    res.populate(&ra_tree);
    ra_tree.write("output_logical_plan.txt").ok();
}

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
            Column {
                name: "column3".to_string(),
                alias: None,
                table: None,
                function: None,
            },
            Column {
                name: "column4".to_string(),
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
            Column {
                name: "column3".to_string(),
                alias: None,
                table: None,
                function: None,
            },
            Column {
                name: "column4".to_string(),
                alias: None,
                table: None,
                function: None,
            },
        ],
    );
    schema
}
