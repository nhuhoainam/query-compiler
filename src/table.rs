use std::fmt;


use crate::common::TreeNode;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Table {
    pub name: String,
    pub alias: Option<String>,
    pub schema: Option<String>,
}

impl TreeNode for Table {
    fn populate(&self) {
        match self.alias {
            Some(ref alias) => {
                add_branch!("{}", alias);
                add_leaf!("{}", self.name);
            }
            None => add_leaf!("{}", self.name),
        }
    }
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
