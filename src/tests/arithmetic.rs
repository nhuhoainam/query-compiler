#[cfg(test)]
mod tests {
    use crate::{
        arithmetic::{
            arithmetic_expression, Arithmetic, ArithmeticBase, ArithmeticExpression,
            ArithmeticOperand, ArithmeticOperator,
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
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Add,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Multiply,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                    right: ArithmeticOperand::Base(ArithmeticBase::Bracketed(Box::new(
                        Arithmetic {
                            operator: ArithmeticOperator::Add,
                            left: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(4),
                            )),
                            right: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(5),
                            )),
                        },
                    ))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Subtract,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("foo"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("baz"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(4))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(5))),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("foo"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("baz"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                },
                alias: None,
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Divide,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: None,
            },
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
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Add,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                },
                alias: Some(String::from("foo")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Multiply,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                    right: ArithmeticOperand::Base(ArithmeticBase::Bracketed(Box::new(
                        Arithmetic {
                            operator: ArithmeticOperator::Add,
                            left: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(4),
                            )),
                            right: ArithmeticOperand::Base(ArithmeticBase::Scalar(
                                Literal::Integer(5),
                            )),
                        },
                    ))),
                },
                alias: Some(String::from("bar")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Subtract,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("foo"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("baz"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                },
                alias: Some(String::from("baz")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: Some(String::from("qux")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Add,
                    left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(1))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                },
                alias: Some(String::from("foo")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(4))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(5))),
                },
                alias: Some(String::from("bar")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Subtract,
                    left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                        name: String::from("foo"),
                        alias: None,
                        table: None,
                        function: None,
                    })),
                    right: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Multiply,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("baz"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                    })),
                },
                alias: Some(String::from("baz")),
            },
            ArithmeticExpression {
                ari: Arithmetic {
                    operator: ArithmeticOperator::Divide,
                    left: ArithmeticOperand::Expression(Box::new(Arithmetic {
                        operator: ArithmeticOperator::Divide,
                        left: ArithmeticOperand::Base(ArithmeticBase::Column(Column {
                            name: String::from("bar"),
                            alias: None,
                            table: None,
                            function: None,
                        })),
                        right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(2))),
                    })),
                    right: ArithmeticOperand::Base(ArithmeticBase::Scalar(Literal::Integer(3))),
                },
                alias: Some(String::from("qux")),
            },
        ];
        for (i, input) in inputs.iter().enumerate() {
            let res = arithmetic_expression(input.as_bytes());
            assert_eq!(res.unwrap().1, expected_outputs[i]);
        }
    }
}
