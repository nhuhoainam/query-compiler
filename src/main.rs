use display_tree::{AsTree, CharSet, StyleBuilder};
use query_compiler::arithmetic::arithmetic_expression;

fn main() {
    let (_, res) = arithmetic_expression(b"1 + (2 + 3) + foo AS sth").unwrap();
    print!(
        "{}",
        AsTree::new(&res)
            .indentation(1)
            .char_set(CharSet::SINGLE_LINE)
    );
}
