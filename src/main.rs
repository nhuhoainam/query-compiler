use debug_tree::defer_print;
use query_compiler::{select::select_statement, common::TreeNode};

fn main() {
    // let (_, res) = join_clause(b"inner join foo on foo.a = bar.a AND foo.a <> 1").unwrap();
    // let (_, res) = literal_expression(b"\'sth\' AS sth_else").unwrap();
    let res = select_statement(b"SELECT * FROM Instructor WHERE salary = ( SELECT MAX(salary) FROM Instructor );");
    match res {
        Ok(out) => out.1.populate(),
        Err(e) => print!("{:?}", e),
    }
    defer_print!();
    // print!("{:?}", res);
}
