use std::fs;

use debug_tree::{defer_print, defer_write};
use query_compiler::{select::select_statement, common::TreeNode};

fn main() {
    // let (_, res) = join_clause(b"inner join foo on foo.a = bar.a AND foo.a <> 1").unwrap();
    // let (_, res) = literal_expression(b"\'sth\' AS sth_else").unwrap();
    let input = fs::read_to_string("input.txt").unwrap();
    let res = select_statement(input.as_bytes());
    match res {
        Ok(out) => out.1.populate(),
        Err(e) => print!("{:?}", e),
    }
    defer_write!("output.txt");
    // print!("{:?}", res);
}
