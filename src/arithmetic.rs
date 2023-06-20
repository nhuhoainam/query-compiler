use crate::common::{Column, Literal};

pub struct Arithmetic {
    pub left: Box<ArithmeticOperand>,
    pub right: Box<ArithmeticOperand>,
    pub operator: ArithmeticOperator,
}

pub enum ArithmeticExpressionBase {
    Column(Column),
    Literal(Literal),
    Bracketed(Box<Arithmetic>)
}

pub enum ArithmeticOperand {
    Base(ArithmeticExpressionBase),
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