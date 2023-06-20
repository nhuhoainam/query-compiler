use crate::arithmetic::ArithmeticExpression;

pub struct Column {
    pub name: String,
    pub alias: Option<String>,
    pub table: Option<String>,
}

pub enum FieldDefinitionExpression {
    All,
    AllFromTable(String),
    Column(Column),
    FieldValue(FieldValueExpression),
}

pub enum FieldValueExpression {
    Arithmetic(ArithmeticExpression),
    Literal(LiteralExpression),
}

pub enum Operator {
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Like,
    NotLike,
    In,
    NotIn,
    Is,
}

pub enum Literal {
    Null,
    String(String),
    Integer(i64),
    Decimal(f64),
    DateTime(String),
}

pub struct LiteralExpression {
    pub value: Literal,
    pub alias: Option<String>,
}