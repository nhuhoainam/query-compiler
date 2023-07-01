#[cfg(test)]
mod tests {
    use crate::{
        arithmetic::{
            arithmetic_expression, Arithmetic, ArithmeticBase, ArithmeticExpression,
            ArithmeticOperator,
        },
        column::Column,
        common::Literal,
    };

    #[test]
    fn simple_arithmetic() {
        let inputs = ["1 + 1", "3 * 4", "foo - bar", "bar / 2"];
        let expected_outputs = [
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticOperator::Add,
                None,
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(3)),
                ArithmeticBase::Scalar(Literal::Integer(4)),
                ArithmeticOperator::Multiply,
                None,
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("foo"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticOperator::Subtract,
                None,
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Scalar(Literal::Integer(2)),
                ArithmeticOperator::Divide,
                None,
            ),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }

    #[test]
    fn arithmetic_with_alias() {
        let inputs = [
            "1 + 1 as foo",
            "3 * 4 as bar",
            "foo - bar as baz",
            "bar / 2 as qux",
        ];
        let expected_outputs = [
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticBase::Scalar(Literal::Integer(1)),
                ArithmeticOperator::Add,
                Some(String::from("foo")),
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Scalar(Literal::Integer(3)),
                ArithmeticBase::Scalar(Literal::Integer(4)),
                ArithmeticOperator::Multiply,
                Some(String::from("bar")),
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("foo"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticOperator::Subtract,
                Some(String::from("baz")),
            ),
            ArithmeticExpression::new(
                ArithmeticBase::Column(Column {
                    name: String::from("bar"),
                    alias: None,
                    table: None,
                    function: None,
                }),
                ArithmeticBase::Scalar(Literal::Integer(2)),
                ArithmeticOperator::Divide,
                Some(String::from("qux")),
            ),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }

    #[test]
    fn nested_arithmetic() {
        let inputs = [
            "1 + 1 + 1",
            "3 * (4 + 5)",
            "foo - bar + baz",
            "bar / 2 + 3",
            "1 + 1 * 2",
            "3 * 4 - 5",
            "foo - bar * baz",
            "bar / 2 / 3",
        ];
        let expected_outputs = [
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Add,
                left: Box::new(Arithmetic::Expr {
                    operator: ArithmeticOperator::Add,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        1,
                    )))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        1,
                    )))),
                }),
                right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                    1,
                )))),
            }),
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Multiply,
                left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                    3,
                )))),
                right: Box::new(Arithmetic::Base(ArithmeticBase::Bracketed(Box::new(
                    Arithmetic::Expr {
                        operator: ArithmeticOperator::Add,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                            4,
                        )))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                            Literal::Integer(5),
                        ))),
                    },
                )))),
            }),
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Add,
                left: Box::new(Arithmetic::Expr {
                    operator: ArithmeticOperator::Subtract,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("foo"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("bar"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                }),
                right: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                    name: String::from("baz"),
                    alias: None,
                    table: None,
                    function: None,
                }))),
            }),
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Add,
                left: Box::new(Arithmetic::Expr {
                    operator: ArithmeticOperator::Divide,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("bar"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        2,
                    )))),
                }),
                right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                    3,
                )))),
            }),
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Add,
                left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                    1,
                )))),
                right: Box::new(Arithmetic::Expr {
                    operator: ArithmeticOperator::Multiply,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        1,
                    )))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        2,
                    )))),
                }),
            }),
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Subtract,
                left: Box::new(Arithmetic::Expr {
                    operator: ArithmeticOperator::Multiply,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        3,
                    )))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        4,
                    )))),
                }),
                right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                    5,
                )))),
            }),
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Subtract,
                left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                    name: String::from("foo"),
                    alias: None,
                    table: None,
                    function: None,
                }))),
                right: Box::new(Arithmetic::Expr {
                    operator: ArithmeticOperator::Multiply,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("bar"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("baz"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                }),
            }),
            ArithmeticExpression::WithoutAlias(Arithmetic::Expr {
                operator: ArithmeticOperator::Divide,
                left: Box::new(Arithmetic::Expr {
                    operator: ArithmeticOperator::Divide,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("bar"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        2,
                    )))),
                }),
                right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                    3,
                )))),
            }),
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }

    #[test]
    fn nested_arithmetic_with_alias() {
        let inputs = [
            "1 + 1 + 1 as foo",
            "3 * (4 + 5) as bar",
            "foo - bar + baz as baz",
            "bar / 2 + 3 as qux",
            "1 + 1 * 2 as foo",
            "3 * 4 - 5 as bar",
            "foo - bar * baz as baz",
            "bar / 2 / 3 as qux",
        ];

        let expected_outputs = [
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Add,
                    left: Box::new(Arithmetic::Expr {
                        operator: ArithmeticOperator::Add,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                            1,
                        )))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                            Literal::Integer(1),
                        ))),
                    }),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        1,
                    )))),
                },
                alias: String::from("foo"),
            },
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Multiply,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        3,
                    )))),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Bracketed(Box::new(
                        Arithmetic::Expr {
                            operator: ArithmeticOperator::Add,
                            left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                                Literal::Integer(4),
                            ))),
                            right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                                Literal::Integer(5),
                            ))),
                        },
                    )))),
                },
                alias: String::from("bar"),
            },
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Add,
                    left: Box::new(Arithmetic::Expr {
                        operator: ArithmeticOperator::Subtract,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                            name: String::from("foo"),
                            alias: None,
                            table: None,
                            function: None,
                        }))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        }))),
                    }),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("baz"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                },
                alias: String::from("baz"),
            },
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Add,
                    left: Box::new(Arithmetic::Expr {
                        operator: ArithmeticOperator::Divide,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        }))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                            Literal::Integer(2),
                        ))),
                    }),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        3,
                    )))),
                },
                alias: String::from("qux"),
            },
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Add,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        1,
                    )))),
                    right: Box::new(Arithmetic::Expr {
                        operator: ArithmeticOperator::Multiply,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                            1,
                        )))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                            Literal::Integer(2),
                        ))),
                    }),
                },
                alias: String::from("foo"),
            },
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Subtract,
                    left: Box::new(Arithmetic::Expr {
                        operator: ArithmeticOperator::Multiply,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                            3,
                        )))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                            Literal::Integer(4),
                        ))),
                    }),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        5,
                    )))),
                },
                alias: String::from("bar"),
            },
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Subtract,
                    left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                        name: String::from("foo"),
                        alias: None,
                        table: None,
                        function: None,
                    }))),
                    right: Box::new(Arithmetic::Expr {
                        operator: ArithmeticOperator::Multiply,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        }))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                            name: String::from("baz"),
                            alias: None,
                            table: None,
                            function: None,
                        }))),
                    }),
                },
                alias: String::from("baz"),
            },
            ArithmeticExpression::WithAlias {
                ari: Arithmetic::Expr {
                    operator: ArithmeticOperator::Divide,
                    left: Box::new(Arithmetic::Expr {
                        operator: ArithmeticOperator::Divide,
                        left: Box::new(Arithmetic::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        }))),
                        right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(
                            Literal::Integer(2),
                        ))),
                    }),
                    right: Box::new(Arithmetic::Base(ArithmeticBase::Scalar(Literal::Integer(
                        3,
                    )))),
                },
                alias: String::from("qux"),
            },
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }
}
