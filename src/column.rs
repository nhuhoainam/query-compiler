use std::fmt;

use crate::keywords::escape_if_keyword;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum FunctionExpression {
    Avg(Column, bool),
    Count(Column, bool),
    CountStar,
    Sum(Column, bool),
    Max(Column),
    Min(Column),
    GroupConcat(Column, String),
    Generic(String, Columns),
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FunctionExpression::Avg(ref col, d) if d => write!(f, "avg(distinct {})", col),
            FunctionExpression::Count(ref col, d) if d => write!(f, "count(distinct {})", col),
            FunctionExpression::Sum(ref col, d) if d => write!(f, "sum(distinct {})", col),

            FunctionExpression::Avg(ref col, _) => write!(f, "avg({})", col),
            FunctionExpression::Count(ref col, _) => write!(f, "count({})", col),
            FunctionExpression::CountStar => write!(f, "count(*)"),
            FunctionExpression::Sum(ref col, _) => write!(f, "sum({})", col),
            FunctionExpression::Max(ref col) => write!(f, "max({})", col),
            FunctionExpression::Min(ref col) => write!(f, "min({})", col),
            FunctionExpression::GroupConcat(ref col, ref s) => {
                write!(f, "group_concat({}, {})", col, s)
            }
            FunctionExpression::Generic(ref name, ref args) => write!(f, "{}({})", name, args),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Columns {
    pub arguments: Vec<Column>,
}

impl fmt::Display for Columns {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.arguments
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<String>>()
                .join(",")
        )?;
        Ok(())
    }
}

impl<'a> From<Vec<Column>> for Columns {
    fn from(args: Vec<Column>) -> Columns {
        Columns { arguments: args }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Column {
    pub name: String,
    pub alias: Option<String>,
    pub table: Option<String>,
    pub function: Option<Box<FunctionExpression>>,
}

impl fmt::Display for Column {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref table) = self.table {
            write!(
                f,
                "{}.{}",
                escape_if_keyword(table),
                escape_if_keyword(&self.name)
            )?;
        } else if let Some(ref function) = self.function {
            write!(f, "{}", *function)?;
        } else {
            write!(f, "{}", escape_if_keyword(&self.name))?;
        }
        if let Some(ref alias) = self.alias {
            write!(f, " AS {}", escape_if_keyword(alias))?;
        }
        Ok(())
    }
}

impl<'a> From<&'a str> for Column {
    fn from(c: &str) -> Column {
        match c.find(".") {
            None => Column {
                name: String::from(c),
                alias: None,
                table: None,
                function: None,
            },
            Some(i) => Column {
                name: String::from(&c[i + 1..]),
                alias: None,
                table: Some(String::from(&c[0..i])),
                function: None,
            },
        }
    }
}