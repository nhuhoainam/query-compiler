use display_tree::{AsTree, CharSet, StyleBuilder};
use query_compiler::{arithmetic::arithmetic_expression, common::literal_expression, join::join_clause};

fn main() {
    let (_, res) = join_clause(b"inner join foo on foo.a = bar.a AND foo.a <> 1").unwrap();
    // let (_, res) = literal_expression(b"\'sth\' AS sth_else").unwrap();
    print!(
        "{}",
        AsTree::new(&res)
            .indentation(1)
            .char_set(CharSet::SINGLE_LINE)
    );
    // print!("{:?}", res);
}
