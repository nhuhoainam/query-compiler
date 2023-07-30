use std::fmt;

use crate::{common::TreeNode, keywords::escape_if_keyword};

type FunctionArgument = Column;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum FunctionExpression {
    Avg(FunctionArgument, bool),
    Count(FunctionArgument, bool),
    CountStar,
    Sum(FunctionArgument, bool),
    Max(FunctionArgument),
    Min(FunctionArgument),
    GroupConcat(FunctionArgument, String),
    Generic(String, FunctionArguments),
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
pub struct FunctionArguments {
    pub arguments: Vec<FunctionArgument>,
}

impl fmt::Display for FunctionArguments {
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

impl<'a> From<Vec<FunctionArgument>> for FunctionArguments {
    fn from(args: Vec<FunctionArgument>) -> FunctionArguments {
        FunctionArguments { arguments: args }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Column {
    pub name: String,
    pub alias: Option<String>,
    pub table: Option<String>,
    pub function: Option<Box<FunctionExpression>>,
}

impl TreeNode for Column {
    fn populate(&self) {
        match self.alias {
            Some(ref alias) => match self.function {
                Some(ref fnt) => {
                    add_branch!("{}", escape_if_keyword(alias));
                    add_leaf!("{}", fnt)
                }
                None => {
                    add_branch!("{}", escape_if_keyword(alias));
                    add_leaf!(
                        "{}",
                        if let Some(ref table) = self.table {
                            format!("{}.{}", escape_if_keyword(table), escape_if_keyword(&self.name))
                        } else {
                            escape_if_keyword(&self.name)
                        },
                    );
                }
            },
            None => add_leaf!(
                "{}",
                if let Some(ref table) = self.table {
                    format!("{}.{}", escape_if_keyword(table), escape_if_keyword(&self.name))
                } else {
                    escape_if_keyword(&self.name)
                },
            )
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn column_from_str() {
        let s = "table.col";
        let c = Column::from(s);

        assert_eq!(
            c,
            Column {
                name: String::from("col"),
                alias: None,
                table: Some(String::from("table")),
                function: None,
            }
        );
    }

    #[test]
    fn print_function_column() {
        let c1 = Column {
            name: "".into(), // must be present, but will be ignored
            alias: Some("foo".into()),
            table: None,
            function: Some(Box::new(FunctionExpression::CountStar)),
        };
        let c2 = Column {
            name: "".into(), // must be present, but will be ignored
            alias: None,
            table: None,
            function: Some(Box::new(FunctionExpression::CountStar)),
        };
        let c3 = Column {
            name: "".into(), // must be present, but will be ignored
            alias: None,
            table: None,
            function: Some(Box::new(FunctionExpression::Sum(
                Column::from("mytab.foo"),
                false,
            ))),
        };

        assert_eq!(format!("{}", c1), "count(*) AS foo");
        assert_eq!(format!("{}", c2), "count(*)");
        assert_eq!(format!("{}", c3), "sum(mytab.foo)");
    }
}
