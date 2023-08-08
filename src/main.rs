use std::fs;

use debug_tree::TreeBuilder;
use query_compiler::{compound_select::compound_selection, common::TreeNode, select::select_statement};

fn main() {
    // let (_, res) = join_clause(b"inner join foo on foo.a = bar.a AND foo.a <> 1").unwrap();
    // let (_, res) = literal_expression(b"\'sth\' AS sth_else").unwrap();
    let input = fs::read_to_string("input.txt").unwrap();
    let res = select_statement(input.as_bytes());
    let tree = TreeBuilder::new();
    match res {
        Ok(out) => out.1.populate(&tree),
        Err(e) => print!("{:?}", e),
    }
    tree.write("output.txt").ok();
    // print!("{:?}", res);
}
