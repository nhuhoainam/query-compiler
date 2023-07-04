use display_tree::{AsTree, CharSet, StyleBuilder};
use query_compiler::{arithmetic::arithmetic_expression, common::literal_expression};

fn main() {
    let (_, res) = arithmetic_expression(b"1 + (2 + 3) + foo AS sth").unwrap();
    // let (_, res) = literal_expression(b"\'sth\' AS sth_else").unwrap();
    print!(
        "{}",
        AsTree::new(&res)
            .indentation(1)
            .char_set(CharSet::SINGLE_LINE)
    );
    // print!("{:?}", res);
}
