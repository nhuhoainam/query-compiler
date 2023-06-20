pub struct Attribute {
    pub value: String,
    pub alias: Option<String>,
}

pub enum FieldDefinition {
    All,
    AllFromTable(Relation),
    Column(Attribute),
}

pub struct Relation {
    pub value: String,
    pub alias: Option<String>,
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

