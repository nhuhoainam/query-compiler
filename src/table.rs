use std::fmt;

use display_tree::DisplayTree;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Table {
    pub name: String,
    pub alias: Option<String>,
    pub schema: Option<String>,
}

impl From<String> for Table {
    fn from(name: String) -> Self {
        Table {
            name,
            alias: None,
            schema: None,
        }
    }
}

impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.alias {
            Some(a) => write!(f, "{} AS {}", self.name, a),
            None => write!(f, "{}", self.name),
        }
    }
}

impl DisplayTree for Table {
    fn fmt(&self, f: &mut fmt::Formatter, style: display_tree::Style) -> fmt::Result {
        let table_name = match self.schema {
            Some(ref schema) => format!("{}.{}", schema, self.name),
            None => self.name.clone(),
        };
        match self.alias {
            Some(ref alias) => {
                writeln!(f, "{}", alias)?;
                write!(
                    f,
                    "{}{} {}",
                    style.char_set.end_connector,
                    std::iter::repeat(style.char_set.horizontal)
                        .take(style.indentation as usize)
                        .collect::<String>(),
                    table_name
                )
            }
            None => write!(f, "{}", table_name),
        }
    }
}