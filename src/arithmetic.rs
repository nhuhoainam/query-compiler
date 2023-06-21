use crate::common::{Column, Literal};

pub struct Arithmetic {
    pub operator: ArithmeticOperator,
    pub left: ArithmeticOperand,
    pub right: ArithmeticOperand,
}

pub enum ArithmeticBase {
    Column(Column),
    Literal(Literal),
    Bracketed(Box<Arithmetic>)
}

pub enum ArithmeticOperand {
    Base(ArithmeticBase),
    Expression(Box<Arithmetic>),
}

pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub struct ArithmeticExpression {
    pub ari: Arithmetic,
    pub alias: Option<String>,
}

impl Arithmetic {
    pub fn new(operator: ArithmeticOperator, left: ArithmeticBase, right: ArithmeticBase) -> Self {
        Self {
            operator,
            left: ArithmeticOperand::Base(left),
            right: ArithmeticOperand::Base(right),
        }
    }
}

impl ArithmeticExpression {
    pub fn new(
        left: ArithmeticBase,
        right: ArithmeticBase,
        operator: ArithmeticOperator,
        alias: Option<String>,
    ) -> Self {
        Self {
            ari: Arithmetic {
                operator,
                left: ArithmeticOperand::Base(left),
                right: ArithmeticOperand::Base(right),
            },
            alias,
        }
    }
}