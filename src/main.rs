use std::fs;

use debug_tree::TreeBuilder;
use query_compiler::{
    common::TreeNode,
    compound_select::compound_selection,
    logical_plan::Relation,
    schema::{print_schema, schema, Schema},
};

fn main() {
    run_query();
}

fn run_query() {
    let input = fs::read_to_string("input.txt").unwrap();
    let parse_res = compound_selection(input.as_bytes());
    let parse_tree = TreeBuilder::new();
    let parse = parse_res.unwrap().1;
    parse.populate(&parse_tree);
    parse_tree.write("output_parse_tree.txt").ok();
    let schema = get_schema();
    let res: Relation = (parse, schema).into();
    print!("{:#?}", res);
    let ra_tree = TreeBuilder::new();
    res.populate(&ra_tree);
    ra_tree.write("output_logical_plan.txt").ok();
}

fn get_schema() -> Schema {
    let input = fs::read_to_string("schema.txt").unwrap();
    let schema = schema(input.as_bytes()).unwrap().1;
    print_schema(schema.clone());
    schema
}
