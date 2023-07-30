use debug_tree::defer_print;
use query_compiler::{arithmetic::arithmetic_expression, common::{literal_expression, TreeNode}, join::join_clause, select::select_statement, condition::condition_expr};

fn main() {
    // let (_, res) = join_clause(b"inner join foo on foo.a = bar.a AND foo.a <> 1").unwrap();
    // let (_, res) = literal_expression(b"\'sth\' AS sth_else").unwrap();
    let (_, res) = select_statement(b"SELECT ID, name FROM Student WHERE ID NOT IN ( SELECT DISTINCT ID FROM Takes WHERE Takes.[year] < '2023' );").unwrap();
    // let (_, res1) = condition_expr(b"salary = ( SELECT MAX(salary) FROM Instructor )").unwrap();
    defer_print!();
    res.populate();
    // print!(
    //     "{}",
    //     AsTree::new(&res)
    //         .indentation(1)
    //         .char_set(CharSet::SINGLE_LINE)
    // );
    // print!("{:?}", res);
}
