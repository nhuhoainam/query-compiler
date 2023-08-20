use std::fmt;


use debug_tree::TreeBuilder;

use crate::common::TreeNode;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Table {
    pub name: String,
    pub alias: Option<String>,
    pub schema: Option<String>,
}

impl TreeNode for Table {
    fn populate(&self, parent: &TreeBuilder) {
        match self.alias {
            Some(ref alias) => {
                let mut branch = parent.add_branch(format!("{}", alias).as_str());
                parent.add_leaf(format!("{}", self.name).as_str());
                branch.release();
            }
            None => parent.add_leaf(format!("{}", self.name).as_str()),
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


impl<'a> From<&'a str> for Table {
    fn from(t: &str) -> Table {
        Table {
            name: String::from(t),
            alias: None,
            schema: None,
        }
    }
}

impl<'a> From<(&'a str, &'a str)> for Table {
    fn from(t: (&str, &str)) -> Table {
        Table {
            name: String::from(t.1),
            alias: None,
            schema: Some(String::from(t.0)),
        }
    }
}
