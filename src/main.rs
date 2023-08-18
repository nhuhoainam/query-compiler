use std::{collections::BTreeMap, fs};

use debug_tree::TreeBuilder;
use query_compiler::{
    column::Column,
    common::TreeNode,
    compound_select::compound_selection,
    logical_plan::{Relation, Schema},
    select::select_statement,
};

fn main() {
    // let input = fs::read_to_string("input.txt").unwrap();
    // let res = compound_selection(input.as_bytes());
    // let tree = TreeBuilder::new();
    // match res {
    //     Ok(out) => out.1.populate(&tree),
    //     Err(e) => print!("{:?}", e),
    // }
    // tree.write("output.txt").ok();
    let sql = b"SELECT column1 FROM table";
    let parse_tree = select_statement(sql).unwrap().1;
    let mut schema: Schema = BTreeMap::new();
    schema.insert(
        "table".to_string(),
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
        ],
    );
    let res: Relation = (parse_tree, schema).into();
    print!("{:#?}", res);
}
