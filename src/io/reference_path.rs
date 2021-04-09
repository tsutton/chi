//! ReferencePath is a reimplementation of the subset of JSONPath that is allowed to be used in fields such as ResultPath and ItemsPath.
//!
//! This subset consists of only array-lookup and field-access operations, like `$.field['field2'][3]`.
//! Unfortunately, there is no spec for JSONPath, and the States spec itself is confusing regarding what is and isn't allowed.
//! The rules this module follows are:
//! - A reference path consists of a '$' followed by any number (possibly 0) of selectors.
//! - A selector is either an index selector, a dotted field selector, or a bracketed field selector.
//! - an index selector is a u32-sized base 10 literal within brackets, such as `[1234]`. Leading zeros are allowed.
//! - A bracketed field selector is enclosed by square brackets and single quotes, like `['field']`.
//!   Within the single quotes, escape sequences \', \", \\, and \/ each refer to the second character.
//!   Escapes \n, \b, \r, \t, and \f are also recognized.
//!   Backslash followed by anything else is invalid. Any other UTF-8 is allowed, including literal spaces and newlines.
//! - A dotted field selector is a period (`.`) followed by any UTF-8 characeters except for space (` `), period (`.`) or
//!   open-bracket (`[`). No escape sequences are recognized, however you can have literal newlines.
//!
//! This behavior follows Jayway JSONPath, as close as I can tell. If this and Jayway disagree, it is a bug here.
//! Not all examples on the States spec are considered valid.
//! For example, the spec says that `$.\stor\e\.boo\k` is a valid reference path; I have no idea what \s or \e or \k means.

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, one_of},
    combinator::{eof, map, recognize, value},
    multi::{fold_many1, many0, many1},
    sequence::{delimited, preceded},
    IResult,
};

use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{json, Value};
use thiserror::Error;

/// A reference path is a particular kind of JSONPath which consists only of array indexing and field selection.
///
/// For example, the JSONPath `$.field[1]['index]` is a valid reference path, but `$..`, `$[1:4]` and anything involving `?` are not.
/// The constructor for ReferencePath is `ReferencePath::compile`. Once compiled, it can be used to extract the value at that path from
/// a `serde_json::Value` (as an mutable or immutable borrow) using the `select` and `select_mut` methods.
/// It can also be used to set the value at the given path within a `serde_json::Value`, creating nodes that don't exist, but erroring if
/// encountering an existing field that isn't of type object.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ReferencePath(Vec<Select>);

impl<'de> Deserialize<'de> for ReferencePath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: &str = <&str as Deserialize<'de>>::deserialize(deserializer)?;
        let path = ReferencePath::compile(s).map_err(serde::de::Error::custom)?;
        Ok(path)
    }
}

// TODO this doesn't re-escape Select::Field's quotes and such
impl Serialize for ReferencePath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut composed = String::from("$");
        self.0
            .iter()
            .map(|s| match s {
                Select::Index(i) => format!("[{}]", i),
                Select::Field(s) => format!("['{}']", s),
            })
            .for_each(|s| composed.push_str(&s));
        serializer.serialize_str(&composed)
    }
}

impl Default for ReferencePath {
    fn default() -> Self {
        ReferencePath(vec![])
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum Select {
    Index(usize),
    Field(String),
}

impl Select {
    fn select<'a>(&self, value: &'a Value) -> Option<&'a Value> {
        match self {
            Select::Index(idx) => value.get(idx),
            Select::Field(s) => value.get(s),
        }
    }
    fn select_mut<'a>(&self, value: &'a mut Value) -> Option<&'a mut Value> {
        match self {
            Select::Index(idx) => value.get_mut(idx),
            Select::Field(s) => value.get_mut(s),
        }
    }
}

/// ReferencePathError captures the different types of errors that can occur when creating or using ReferencePaths
#[derive(Debug, Error)]
pub enum ReferencePathError {
    /// ParseError is returned when trying to compile an invalid path.
    #[error("error while parsing reference path: {0}")]
    ParseError(String),
    #[error("error while applying reference path: {0}")]
    PathError(String),
}

impl<'a> From<nom::Err<nom::error::Error<&'a str>>> for ReferencePathError {
    fn from(base: nom::Err<nom::error::Error<&'a str>>) -> Self {
        ReferencePathError::ParseError(base.to_string())
    }
}

impl ReferencePath {
    /// Compiles a string into a ReferencePath, returning either the validated path type or an error.
    pub fn compile(path: &str) -> Result<ReferencePath, ReferencePathError> {
        Ok(parse_path(path)?.1)
    }

    /// Select a node from the given value using this selector, borrowing the node if it exists,
    /// or returning None if the selector failed to apply.
    ///
    /// Note that this is slightly different than Jayway with SUPRESS_EXCEPTIONS; in that case,
    /// null would be returned instead of None; by using an Option we can better express the lifetimes.
    pub fn select<'a>(&self, value: &'a Value) -> Option<&'a Value> {
        // This is a fold over self's vec of selects, starting with Some(value), continually applying
        // the select. If any select fails (i.e. returns None), subsequent selectors will not be applied
        // (due to the use of and_then), and at the end, None gets mapped to Null.
        self.0.iter().fold(Some(value), |current, selector| {
            current.and_then(|val| selector.select(val))
        })
    }

    /// Mutably select a node from the given value using this selector, as in `select`.
    pub fn select_mut<'a>(
        &self,
        value: &'a mut serde_json::Value,
    ) -> Option<&'a mut serde_json::Value> {
        // See select for what this does.
        self.0.iter().fold(Some(value), |current, selector| {
            current.and_then(|val| selector.select_mut(val))
        })
    }

    pub fn insert(
        &self,
        target: &mut serde_json::Value,
        new_value: serde_json::Value,
    ) -> Result<(), ReferencePathError> {
        // Iterate over self.0, until either
        // - we try to index or lookup on a mismatching type - then fail
        // - we are in an object, and the field doesn't exist
        // - we get to the end
        let node =
            self.0
                .iter()
                .try_fold(target, |current, selector| match (current, selector) {
                    (Value::Array(arr), Select::Index(idx)) => arr.get_mut(*idx).ok_or_else(|| {
                        // This error message could be better, maybe be tossing an .enumerate() on the top .iter()
                        // and using it to construct where exactly the problem was
                        ReferencePathError::PathError(
                            "out of bounds array index in path application".to_owned(),
                        )
                    }),
                    // We can never fail when doing a field index into an object; if the field doesn't exist, create it
                    (Value::Object(map), Select::Field(m)) => Ok(map.entry(m).or_insert(json!({}))),
                    _ => Err(ReferencePathError::PathError(
                        // Error message can be better, see above.
                        "type mismatch while processing path".to_owned(),
                    )),
                })?;
        *node = new_value;
        Ok(())
    }
}

fn parse_path(i: &str) -> IResult<&str, ReferencePath> {
    let (i, _) = char('$')(i)?;
    let (i, selectors) = many0(parse_select)(i)?;
    eof(i)?;
    Ok((i, ReferencePath(selectors)))
}

fn parse_select(i: &str) -> IResult<&str, Select> {
    alt((parse_dot_select, parse_bracket_select))(i)
}

fn parse_dot_select(i: &str) -> IResult<&str, Select> {
    // So far as I can tell by playing around with examples,
    // Jayway allows any non-space, non-. characters
    // and it has no special escaping rules, so "$.foo\nbar" (an actual newline in the path)
    // will match against `{"foo\nbar": 5}` (a newline in the JSON field).
    let (i, _) = char('.')(i)?;
    // let (i, identifier) = many1(none_of(" ."))(i)?;
    let (i, identifier) = take_while1(|c| c != '.' && c != ' ' && c != '[')(i)?;
    Ok((i, Select::Field(identifier.to_owned())))
}

fn parse_bracket_select(i: &str) -> IResult<&str, Select> {
    delimited(
        tag("["),
        alt((
            delimited(char('\''), parse_bracket_select_inner_string, char('\'')),
            parse_bracket_select_inner_number,
        )),
        tag("]"),
    )(i)
}

// Parsing strings with escape sequences is based on the example from nom:
// https://github.com/Geal/nom/blob/master/examples/string.rs

// As we parse, we will build up a sequence of these fragments.
// If there are no escapes, we can get the whole output as a borrow from the input.
// But, because we need to convert r"\n" into "\n", we need a place to shove the "\n"
// (since it's not present in the input), thus we need this enum.
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
}

fn parse_literal(i: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '\'' && c != '\\')(i)
}

fn parse_escape(i: &str) -> IResult<&str, char> {
    // It appears that we apply the escaping rules to the input:
    // - \n, \t, \b, \n, \r, \t mean their normal meanings
    // - \\, \/, \", \' mean the character after the leading slash
    // (note \' is unique here, it's not an escape in JSON)
    preceded(
        char('\\'),
        alt((
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
            value('\'', char('\'')),
        )),
    )(i)
}

fn parse_fragment(input: &str) -> IResult<&str, StringFragment> {
    alt((
        map(parse_literal, StringFragment::Literal),
        map(parse_escape, StringFragment::EscapedChar),
    ))(input)
}

fn parse_bracket_select_inner_string(i: &str) -> IResult<&str, Select> {
    let (i, s) = fold_many1(parse_fragment, String::new(), |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(s),
            StringFragment::EscapedChar(c) => string.push(c),
        }
        string
    })(i)?;
    Ok((i, Select::Field(s)))
}

fn parse_bracket_select_inner_number(i: &str) -> IResult<&str, Select> {
    // Jayway allows leading 0s in integral numbers, even though JSON doesn't.
    let (i, s) = recognize(many1(one_of("0123456789")))(i)?;
    let parsed: usize = s
        .parse()
        .map_err(|_| nom::Err::Error(nom::error::Error::new(i, nom::error::ErrorKind::ParseTo)))?;
    Ok((i, Select::Index(parsed)))
}

#[cfg(test)]
mod test {
    use serde_json::json;

    use super::*;

    #[test]
    fn test_dot_select() {
        assert_eq!(
            parse_dot_select(".select.other"),
            Ok((".other", Select::Field("select".to_owned())))
        );
        assert_eq!(
            parse_dot_select(".select other"),
            Ok((" other", Select::Field("select".to_owned())))
        );
        assert!(parse_dot_select("select").is_err());
        assert_eq!(
            parse_dot_select(".&Ж中\u{d096}"),
            Ok(("", Select::Field("&Ж中\u{d096}".to_owned())))
        );
    }

    #[test]
    fn test_bracket_select() {
        assert_eq!(
            parse_bracket_select("[1234]"),
            Ok(("", Select::Index(1234)))
        );
        assert_eq!(
            parse_bracket_select("[01234]"),
            Ok(("", Select::Index(1234)))
        );
        assert!(
            parse_bracket_select("[]").is_err(),
            "Empty brackets is an error",
        );

        assert_eq!(
            parse_bracket_select("['asdf']"),
            Ok(("", Select::Field("asdf".to_owned())))
        );

        assert_eq!(
            parse_bracket_select("['as\ndf']"),
            Ok(("", Select::Field("as\ndf".to_owned()))),
            "Actual newline character is allowed",
        );

        assert_eq!(
            parse_bracket_select("['as\\ndf']"),
            Ok(("", Select::Field("as\ndf".to_owned()))),
            "Escaped newline also works",
        );
        assert_eq!(
            parse_bracket_select(r#"['as\\df']"#),
            Ok(("", Select::Field(r#"as\df"#.to_owned()))),
            "Escaped backslash works",
        );
        assert_eq!(
            parse_bracket_select(r#"['as\'df']"#),
            Ok(("", Select::Field(r#"as'df"#.to_owned()))),
            "Escaped single quote works",
        );

        assert!(
            parse_bracket_select(r#"['as'df']"#).is_err(),
            "Un-escaped single quote is an error",
        );
        assert!(
            parse_bracket_select(r#"['as\df']"#).is_err(),
            "Un-recognized escape is an error",
        );
        assert!(
            parse_bracket_select(r#"['']"#).is_err(),
            "Empty string is an error",
        );
    }

    #[test]
    fn end_to_end() {
        assert_eq!(
            parse_path(r#"$.field[1]['2']['\' ."']"#),
            Ok((
                "",
                ReferencePath(vec![
                    Select::Field("field".to_owned()),
                    Select::Index(1),
                    Select::Field("2".to_owned()),
                    Select::Field(r#"' .""#.to_owned()),
                ])
            )),
        )
    }

    #[test]
    fn path_select() {
        let mut value = json!({
            "foo": 123,
            "bar": ["a", "b", "c"],
            "car": {
                "cdr": true
            }
        });

        let path = ReferencePath::compile("$.foo").unwrap();
        assert_eq!(path.select(&value).unwrap(), &json!(123));

        let path = ReferencePath::compile("$.bar").unwrap();
        assert_eq!(path.select(&value).unwrap(), &json!(["a", "b", "c"]));

        let path = ReferencePath::compile("$.car.cdr").unwrap();
        assert_eq!(path.select(&value).unwrap(), &json!(true));

        let path = ReferencePath::compile("$.no").unwrap();
        assert_eq!(path.select(&value), None);

        let path = ReferencePath::compile("$.foo").unwrap();
        let selected = path.select_mut(&mut value).unwrap();
        *selected = json!("new value");
        assert_eq!(
            value,
            json!({
                "foo": "new value",
                "bar": ["a", "b", "c"],
                "car": {
                    "cdr": true
                }
            })
        )
    }

    #[test]
    fn insert() {
        let mut value = json!({
            "foo": 123,
            "bar": ["a", "b", "c"],
            "car": {
                "cdr": true
            }
        });

        ReferencePath::compile("$.foo")
            .unwrap()
            .insert(&mut value, json!("hello"))
            .unwrap();
        assert_eq!(
            value,
            json!({
                "foo": "hello",
                "bar": ["a", "b", "c"],
                "car": {
                    "cdr": true
                }
            })
        );

        ReferencePath::compile("$.bar[0]")
            .unwrap()
            .insert(&mut value, json!("hello"))
            .unwrap();
        assert_eq!(
            value,
            json!({
                "foo": "hello",
                "bar": ["hello", "b", "c"],
                "car": {
                    "cdr": true
                }
            })
        );

        ReferencePath::compile("$.car.cdr")
            .unwrap()
            .insert(&mut value, json!("hello"))
            .unwrap();
        assert_eq!(
            value,
            json!({
                "foo": "hello",
                "bar": ["hello", "b", "c"],
                "car": {
                    "cdr": "hello"
                }
            })
        );
        ReferencePath::compile("$.new")
            .unwrap()
            .insert(&mut value, json!("hello"))
            .unwrap();
        assert_eq!(
            value,
            json!({
                        "foo": "hello",
                        "bar": ["hello", "b", "c"],
                        "car": {
                            "cdr": "hello"
                        },
            "new": "hello"
                    })
        );

        assert!(
            ReferencePath::compile("$.new[0]")
                .unwrap()
                .insert(&mut value, json!("hello"))
                .is_err(),
            "integer index into string"
        );
        assert!(
            ReferencePath::compile("$.bar.baz")
                .unwrap()
                .insert(&mut value, json!("hello"))
                .is_err(),
            "field lookup on array"
        );
        assert!(
            ReferencePath::compile("$.bar[3]")
                .unwrap()
                .insert(&mut value, json!("hello"))
                .is_err(),
            "out of bounds array lookup"
        );
    }

    #[test]
    fn reference_path_deserialize() {
        assert_eq!(
            serde_json::from_str::<ReferencePath>("\"$.a.b['c'][1]\"").unwrap(),
            ReferencePath(vec![
                Select::Field("a".to_owned()),
                Select::Field("b".to_owned()),
                Select::Field("c".to_owned()),
                Select::Index(1),
            ])
        );
        assert!(serde_json::from_str::<ReferencePath>("\"$.a.b[c][1]\"").is_err());
        assert!(serde_json::from_str::<ReferencePath>("5").is_err());
        assert!(serde_json::from_str::<ReferencePath>("null").is_err());
        assert!(serde_json::from_str::<ReferencePath>("{}").is_err());
    }

    #[test]
    fn reference_path_serialize() {
        assert_eq!(
            serde_json::to_string(&ReferencePath(vec![
                Select::Field("a".to_owned()),
                Select::Field("b".to_owned()),
                Select::Field("c".to_owned()),
                Select::Index(1),
            ]))
            .unwrap(),
            "\"$['a']['b']['c'][1]\"".to_owned()
        );
    }
}
