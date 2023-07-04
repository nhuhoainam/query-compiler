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