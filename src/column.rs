use std::fmt;

use debug_tree::TreeBuilder;

use crate::{common::TreeNode, keywords::escape_if_keyword};

pub type FunctionArgument = Column;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FunctionExpression {
    Avg(FunctionArgument, bool),
    Count(FunctionArgument, bool),
    CountStar,
    Sum(FunctionArgument, bool),
    Max(FunctionArgument),
    Min(FunctionArgument),
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FunctionExpression::Avg(ref col, _) => write!(f, "AVG({})", col),
            FunctionExpression::Count(ref col, _) => write!(f, "COUNT({})", col),
            FunctionExpression::CountStar => write!(f, "COUNT(*)"),
            FunctionExpression::Sum(ref col, _) => write!(f, "SUM({})", col),
            FunctionExpression::Max(ref col) => write!(f, "MAX({})", col),
            FunctionExpression::Min(ref col) => write!(f, "MIN({})", col)
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Column {
    pub name: String,
    pub alias: Option<String>,
    pub table: Option<String>,
    pub function: Option<Box<FunctionExpression>>,
}

impl TreeNode for Column {
    fn populate(&self, parent: &TreeBuilder) {
        match self.alias {
            Some(ref alias) => match self.function {
                Some(ref fnt) => {
                    let mut branch =
                        parent.add_branch(format!("{}", escape_if_keyword(alias)).as_str());
                    parent.add_leaf(format!("{}", fnt).as_str());
                    branch.release();
                }
                None => {
                    let mut branch =
                        parent.add_branch(format!("{}", escape_if_keyword(alias)).as_str());
                    parent.add_leaf(
                        format!(
                            "{}",
                            if let Some(ref table) = self.table {
                                format!(
                                    "{}.{}",
                                    escape_if_keyword(table),
                                    escape_if_keyword(&self.name)
                                )
                            } else {
                                escape_if_keyword(&self.name)
                            }
                        )
                        .as_str(),
                    );
                    branch.release();
                }
            },
            None => parent.add_leaf(
                format!(
                    "{}",
                    if let Some(ref table) = self.table {
                        format!(
                            "{}.{}",
                            escape_if_keyword(table),
                            escape_if_keyword(&self.name)
                        )
                    } else {
                        escape_if_keyword(&self.name)
                    }
                )
                .as_str(),
            ),
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
}
