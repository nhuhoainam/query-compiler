use debug_tree::TreeBuilder;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case, take, take_while1},
    character::{
        complete::{digit1, line_ending, multispace0, multispace1, alphanumeric1},
        is_alphanumeric,
    },
    combinator::{map, not, opt, peek},
    error::{ErrorKind, ParseError},
    multi::{fold_many0, many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple, separated_pair},
    IResult, InputLength, Parser,
};

use crate::{
    arithmetic::ArithmeticExpression, column::{Column, FunctionExpression, FunctionArgument, FunctionArguments}, keywords::sql_keywords, table::Table,
};
use std::{
    fmt::{self},
    str::{self, FromStr},
};

pub trait TreeNode {
    fn populate(&self, parent: &TreeBuilder);
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum FieldDefinitionExpression {
    All,
    AllFromTable(Table),
    Column(Column),
    FieldValue(FieldValueExpression),
}

impl TreeNode for FieldDefinitionExpression {
    fn populate(&self, parent: &TreeBuilder) {
        match self {
            FieldDefinitionExpression::All => parent.add_leaf("*"),
            FieldDefinitionExpression::AllFromTable(ref table) => {
                parent.add_leaf(format!("{}.{}", table.name, "*").as_str())
            }
            FieldDefinitionExpression::Column(ref col) => col.populate(parent),
            FieldDefinitionExpression::FieldValue(ref fv) => fv.populate(parent),
        };
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum FieldValueExpression {
    Arithmetic(ArithmeticExpression),
    Literal(LiteralExpression),
}

impl TreeNode for FieldValueExpression {
    fn populate(&self, parent: &TreeBuilder) {
        match self {
            FieldValueExpression::Arithmetic(ref ari) => ari.populate(parent),
            FieldValueExpression::Literal(ref lit) => lit.populate(parent),
        }
    }
}

impl fmt::Display for FieldValueExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FieldValueExpression::Arithmetic(ref ari) => write!(f, "{}", ari),
            FieldValueExpression::Literal(ref lit) => write!(f, "{}", lit),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Operator {
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    All(Box<Operator>),
    Any(Box<Operator>),
    Like,
    NotLike,
    In,
    NotIn,
    Is,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Real {
    pub integral: i32,
    pub fractional: i32,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Null,
    String(String),
    Blob(Vec<u8>),
    Integer(i64),
    FixedPoint(Real),
    DateTime(String),
}

impl From<i64> for Literal {
    fn from(i: i64) -> Self {
        Literal::Integer(i)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct LiteralExpression {
    pub value: Literal,
    pub alias: Option<String>,
}

impl TreeNode for LiteralExpression {
    fn populate(&self, parent: &TreeBuilder) {
        match self.alias {
            Some(ref alias) => {
                let mut branch = parent.add_branch(format!("{}", alias).as_str());
                parent.add_leaf(format!("{}", self.value).as_str());
                branch.release()
            }
            None => add_leaf!("{}", self.value),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Literal::Null => write!(f, "NULL"),
            Literal::String(ref s) => write!(f, "'{}'", s),
            Literal::Blob(ref bv) => write!(
                f,
                "{}",
                bv.iter()
                    .map(|v| format!("{:x}", v))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Literal::Integer(ref i) => write!(f, "{}", i),
            Literal::FixedPoint(ref fi) => write!(f, "{}.{}", fi.integral, fi.fractional),
            Literal::DateTime(ref dt) => write!(f, "{}", dt),
        }
    }
}

impl From<Literal> for LiteralExpression {
    fn from(l: Literal) -> Self {
        LiteralExpression {
            value: l,
            alias: None,
        }
    }
}

impl fmt::Display for LiteralExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.alias {
            Some(ref alias) => write!(f, "{} AS {}", self.value.to_string(), alias),
            None => write!(f, "{}", self.value.to_string()),
        }
    }
}

impl fmt::Display for Real {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.integral, self.fractional)
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            Operator::And => "AND",
            Operator::Or => "OR",
            Operator::Like => "LIKE",
            Operator::NotLike => "NOT_LIKE",
            Operator::Equal => "=",
            Operator::NotEqual => "<>",
            Operator::Greater => ">",
            Operator::GreaterOrEqual => ">=",
            Operator::Less => "<",
            Operator::LessOrEqual => "<=",
            Operator::In => "IN",
            Operator::NotIn => "NOT IN",
            Operator::Is => "IS",
            Operator::All(ref inner_op) => return write!(f, "{} ALL", inner_op),
            Operator::Any(ref inner_op) => return write!(f, "{} ANY", inner_op),
        };
        write!(f, "{}", op)
    }
}

#[inline]
pub fn is_sql_identifier(chr: u8) -> bool {
    is_alphanumeric(chr) || chr == '_' as u8 || chr == '@' as u8
}

/// Parse a SQL identifier (alphanumeric1 and "_").
pub fn sql_identifier(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        preceded(not(peek(sql_keywords)), take_while1(is_sql_identifier)),
        delimited(tag("`"), take_while1(is_sql_identifier), tag("`")),
        delimited(tag("["), take_while1(is_sql_identifier), tag("]")),
    ))(i)
}

/// Parse optional parentheses () around expressions
pub(crate) fn opt_delimited<I: Clone, O1, O2, O3, E: ParseError<I>, F, G, H>(
    mut first: F,
    mut second: G,
    mut third: H,
) -> impl FnMut(I) -> IResult<I, O2, E>
where
    F: Parser<I, O1, E>,
    G: Parser<I, O2, E>,
    H: Parser<I, O3, E>,
{
    move |input: I| {
        let inp = input.clone();
        match second.parse(input) {
            Ok((i, o)) => Ok((i, o)),
            _ => {
                let (inp, _) = first.parse(inp)?;
                let (inp, o2) = second.parse(inp)?;
                third.parse(inp).map(|(i, _)| (i, o2))
            }
        }
    }
}

/// Parse rule for whitespace separated comma
pub(crate) fn ws_sep_comma(i: &[u8]) -> IResult<&[u8], &[u8]> {
    delimited(multispace0, tag(","), multispace0)(i)
}

/// Parse rule for whitespace separated equal sign
pub(crate) fn ws_sep_equals<'a, I>(i: I) -> IResult<I, I>
where
    I: nom::InputTakeAtPosition + nom::InputTake + nom::Compare<&'a str>,
    // Compare required by tag
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
    // AsChar and Clone required by multispace0
{
    delimited(multispace0, tag("="), multispace0)(i)
}

/// Parse rule for AS-based aliases for SQL entities.
pub fn as_alias(i: &[u8]) -> IResult<&[u8], &str> {
    map(
        tuple((
            multispace1,
            pair(tag_no_case("as"), multispace1),
            sql_identifier,
        )),
        |a| str::from_utf8(a.2).unwrap(),
    )(i)
}

/// Parse a SQL column identifier in the table.column format
pub fn column_identifier_no_alias(i: &[u8]) -> IResult<&[u8], Column> {
    let table_parser = pair(opt(terminated(sql_identifier, tag("."))), sql_identifier);

    alt((
        map(column_function, |f| Column {
            name: format!("{}", f),
            alias: None,
            table: None,
            function: Some(Box::new(f)),
        }),
        map(table_parser, |tup| Column {
            name: str::from_utf8(tup.1).unwrap().to_string(),
            alias: None,
            table: match tup.0 {
                None => None,
                Some(t) => Some(str::from_utf8(t).unwrap().to_string()),
            },
            function: None,
        })
    ))(i)
}

// Parses the argument for an aggregation function
pub fn function_argument_parser(i: &[u8]) -> IResult<&[u8], FunctionArgument> {
    map(column_identifier_no_alias, |c| c)(i)
}

// Parses the arguments for an aggregation function, and also returns whether the distinct flag is
// present.
pub fn function_arguments(i: &[u8]) -> IResult<&[u8], (FunctionArgument, bool)> {
    let distinct_parser = opt(tuple((tag_no_case("distinct"), multispace1)));
    let (remaining_input, (distinct, args)) = tuple((distinct_parser, function_argument_parser))(i)?;
    Ok((remaining_input, (args, distinct.is_some())))
}

fn group_concat_fx_helper(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let ws_sep = preceded(multispace0, tag_no_case("separator"));
    let (remaining_input, sep) = delimited(
        ws_sep,
        delimited(tag("'"), opt(alphanumeric1), tag("'")),
        multispace0,
    )(i)?;

    Ok((remaining_input, sep.unwrap_or(&[0u8; 0])))
}

fn group_concat_fx(i: &[u8]) -> IResult<&[u8], (Column, Option<&[u8]>)> {
    pair(column_identifier_no_alias, opt(group_concat_fx_helper))(i)
}

fn delim_fx_args(i: &[u8]) -> IResult<&[u8], (FunctionArgument, bool)> {
    delimited(tag("("), function_arguments, tag(")"))(i)
}

pub fn column_function(i: &[u8]) -> IResult<&[u8], FunctionExpression> {
    let delim_group_concat_fx = delimited(tag("("), group_concat_fx, tag(")"));
    alt((
        map(tag_no_case("count(*)"), |_| FunctionExpression::CountStar),
        map(preceded(tag_no_case("count"), delim_fx_args), |args| {
            FunctionExpression::Count(args.0.clone(), args.1)
        }),
        map(preceded(tag_no_case("sum"), delim_fx_args), |args| {
            FunctionExpression::Sum(args.0.clone(), args.1)
        }),
        map(preceded(tag_no_case("avg"), delim_fx_args), |args| {
            FunctionExpression::Avg(args.0.clone(), args.1)
        }),
        map(preceded(tag_no_case("max"), delim_fx_args), |args| {
            FunctionExpression::Max(args.0.clone())
        }),
        map(preceded(tag_no_case("min"), delim_fx_args), |args| {
            FunctionExpression::Min(args.0.clone())
        }),
        map(
            preceded(tag_no_case("group_concat"), delim_group_concat_fx),
            |spec| {
                let (ref col, ref sep) = spec;
                let sep = match *sep {
                    // default separator is a comma, see MySQL manual §5.7
                    None => String::from(","),
                    Some(s) => String::from_utf8(s.to_vec()).unwrap(),
                };
                FunctionExpression::GroupConcat(col.clone(), sep)
            },
        ),
        map(tuple((sql_identifier, multispace0, tag("("), separated_list0(tag(","), delimited(multispace0, function_argument_parser, multispace0)), tag(")"))), |tuple| {
            let (name, _, _, arguments, _) = tuple;
            FunctionExpression::Generic(
                str::from_utf8(name).unwrap().to_string(), 
                FunctionArguments::from(arguments))
        })
    ))(i)
}

/// Parse a SQL column identifier in the table.column format
pub fn column_identifier(i: &[u8]) -> IResult<&[u8], Column> {
    let col_func_no_table = map(pair(column_function, opt(as_alias)), |tup| Column {
        name: match tup.1 {
            None => format!("{}", tup.0),
            Some(a) => String::from(a),
        },
        alias: match tup.1 {
            None => None,
            Some(a) => Some(String::from(a)),
        },
        table: None,
        function: Some(Box::new(tup.0)),
    });
    let col_w_table = map(
        tuple((
            opt(terminated(sql_identifier, tag("."))),
            sql_identifier,
            opt(as_alias),
        )),
        |tup| Column {
            name: str::from_utf8(tup.1).unwrap().to_string(),
            alias: match tup.2 {
                None => None,
                Some(a) => Some(String::from(a)),
            },
            table: match tup.0 {
                None => None,
                Some(t) => Some(str::from_utf8(t).unwrap().to_string()),
            },
            function: None,
        },
    );
    alt((col_func_no_table, col_w_table))(i)
}

/// Parse a reference to a named table, with an optional alias
pub fn table_reference(i: &[u8]) -> IResult<&[u8], Table> {
    map(pair(sql_identifier, opt(as_alias)), |tup| Table {
        name: String::from(str::from_utf8(tup.0).unwrap()),
        alias: match tup.1 {
            Some(a) => Some(String::from(a)),
            None => None,
        },
        schema: None,
        metadata: None,
    })(i)
}

pub(crate) fn eof<I: Copy + InputLength, E: ParseError<I>>(input: I) -> IResult<I, I, E> {
    if input.input_len() == 0 {
        Ok((input, input))
    } else {
        Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Eof)))
    }
}

/// Parse a terminator that ends a SQL 4 .
pub fn statement_terminator(i: &[u8]) -> IResult<&[u8], ()> {
    let (remaining_input, _) =
        delimited(multispace0, alt((tag(";"), line_ending, eof)), multispace0)(i)?;

    Ok((remaining_input, ()))
}

fn binary_comparison_helper(i: &[u8]) -> IResult<&[u8], Operator> {
    alt((
        map(tag_no_case("!="), |_| Operator::NotEqual),
        map(tag_no_case("<>"), |_| Operator::NotEqual),
        map(tag_no_case("<="), |_| Operator::LessOrEqual),
        map(tag_no_case(">="), |_| Operator::GreaterOrEqual),
        map(tag_no_case("="), |_| Operator::Equal),
        map(tag_no_case("<"), |_| Operator::Less),
        map(tag_no_case(">"), |_| Operator::Greater),
    ))(i)
}

/// Parse binary comparison operators
pub fn binary_comparison_operator(i: &[u8]) -> IResult<&[u8], Operator> {
    alt((
        map(tag_no_case("and"), |_| Operator::And),
        map(tag_no_case("or"), |_| Operator::Or),
        map(separated_pair(
            binary_comparison_helper,
            multispace0,
            opt(alt((
                delimited(multispace0, tag_no_case("all"), multispace0), 
                delimited(multispace0, tag_no_case("any"), multispace0),
            ))
        )), |(op, quantifier)| 
            match quantifier {
                Some(b) => match str::from_utf8(b).unwrap().to_lowercase().as_str() {
                    "all" => Operator::All(Box::new(op)),
                    "any" => Operator::Any(Box::new(op)),
                    e => panic!("Error parsing quantifier: {:?}", e),
                },
                None => op,
            }),
        map(tag_no_case("not like"), |_| Operator::NotLike),
        map(tag_no_case("in"), |_| Operator::In),
        map(tag_no_case("not in"), |_| Operator::NotIn),
        map(tag_no_case("is"), |_| Operator::Is),
    ))(i)
}

fn unpack(v: &[u8]) -> i32 {
    i32::from_str(str::from_utf8(v).unwrap()).unwrap()
}

/// Parse floating point literal value
pub fn float_literal(i: &[u8]) -> IResult<&[u8], Literal> {
    map(tuple((opt(tag("-")), digit1, tag("."), digit1)), |tup| {
        Literal::FixedPoint(Real {
            integral: if (tup.0).is_some() {
                -1 * unpack(tup.1)
            } else {
                unpack(tup.1)
            },
            fractional: unpack(tup.3) as i32,
        })
    })(i)
}

/// Parse integer literal value
pub fn integer_literal(i: &[u8]) -> IResult<&[u8], Literal> {
    map(pair(opt(tag("-")), digit1), |tup| {
        let mut intval = i64::from_str(str::from_utf8(tup.1).unwrap()).unwrap();
        if (tup.0).is_some() {
            intval *= -1;
        }
        Literal::Integer(intval)
    })(i)
}

/// String literal value
fn raw_string_quoted(input: &[u8], is_single_quote: bool) -> IResult<&[u8], Vec<u8>> {
    let quote_slice: &[u8] = if is_single_quote { b"\'" } else { b"\"" };
    let double_quote_slice: &[u8] = if is_single_quote { b"\'\'" } else { b"\"\"" };
    let backslash_quote: &[u8] = if is_single_quote { b"\\\'" } else { b"\\\"" };
    delimited(
        tag(quote_slice),
        fold_many0(
            alt((
                is_not(backslash_quote),
                map(tag(double_quote_slice), |_| -> &[u8] {
                    if is_single_quote {
                        b"\'"
                    } else {
                        b"\""
                    }
                }),
                map(tag("\\\\"), |_| &b"\\"[..]),
                map(tag("\\b"), |_| &b"\x7f"[..]),
                map(tag("\\r"), |_| &b"\r"[..]),
                map(tag("\\n"), |_| &b"\n"[..]),
                map(tag("\\t"), |_| &b"\t"[..]),
                map(tag("\\0"), |_| &b"\0"[..]),
                map(tag("\\Z"), |_| &b"\x1A"[..]),
                preceded(tag("\\"), take(1usize)),
            )),
            || Vec::new(),
            |mut acc: Vec<u8>, bytes: &[u8]| {
                acc.extend(bytes);
                acc
            },
        ),
        tag(quote_slice),
    )(input)
}

fn raw_string_single_quoted(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    raw_string_quoted(i, true)
}

fn raw_string_double_quoted(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    raw_string_quoted(i, false)
}

pub fn string_literal(i: &[u8]) -> IResult<&[u8], Literal> {
    map(
        alt((raw_string_single_quoted, raw_string_double_quoted)),
        |bytes| match String::from_utf8(bytes) {
            Ok(s) => Literal::String(s),
            Err(err) => Literal::Blob(err.into_bytes()),
        },
    )(i)
}

// Parse any literal value.
pub fn literal(i: &[u8]) -> IResult<&[u8], Literal> {
    alt((
        float_literal,
        integer_literal,
        string_literal,
        map(tag_no_case("null"), |_| Literal::Null),
    ))(i)
}

pub fn literal_expression(i: &[u8]) -> IResult<&[u8], LiteralExpression> {
    map(
        pair(opt_delimited(tag("("), literal, tag(")")), opt(as_alias)),
        |p| LiteralExpression {
            value: p.0,
            alias: (p.1).map(|a| a.to_string()),
        },
    )(i)
}

pub fn value_list(i: &[u8]) -> IResult<&[u8], Vec<Literal>> {
    many0(delimited(multispace0, literal, opt(ws_sep_comma)))(i)
}

// Parse rule for a comma-separated list of fields without aliases.
pub fn field_list(i: &[u8]) -> IResult<&[u8], Vec<Column>> {
    many0(terminated(column_identifier_no_alias, opt(ws_sep_comma)))(i)
}

pub fn table_list(i: &[u8]) -> IResult<&[u8], Vec<Table>> {
    many0(terminated(table_reference, opt(ws_sep_comma)))(i)
}
