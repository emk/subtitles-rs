//! A very simple version of an HTML data model. This is designed to correspond
//! to the limited HTML features supported in some subtitle formats, and to be
//! easy to sanitize using a whitelist of supported tags and attributes.

use serde::de::Error as DeError;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt;
use std::result;
use std::str::FromStr;

use super::{Error, Result};

/// An HTML fragment.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct Fragment {
    /// The HTML nodes in this fragment.
    pub nodes: Vec<Node>,
}

impl Fragment {
    /// Create an HTML fragment from a plain text node, escaping any special
    /// characters.
    pub fn from_text<S: Into<String>>(text: S) -> Fragment {
        let node = Node::Text { text: text.into() };
        Fragment { nodes: vec![node] }
    }
}

impl FromStr for Fragment {
    type Err = Error;

    fn from_str(html: &str) -> Result<Fragment> {
        Ok(
            grammar::fragment(html).map_err(|err| Error::CouldNotParseHtml {
                html: html.to_owned(),
                source: Box::new(err),
            })?,
        )
    }
}

impl fmt::Display for Fragment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for node in &self.nodes {
            node.fmt(f)?;
        }
        Ok(())
    }
}

impl<'de> Deserialize<'de> for Fragment {
    fn deserialize<D: Deserializer<'de>>(d: D) -> result::Result<Self, D::Error> {
        let raw_html = String::deserialize(d)?;
        raw_html.parse().map_err(|err: Error| {
            // Make sure we get the entire cause chain for this error.
            //
            // TODO: Use a writer to format this, or just use `common_failures`.
            let mut msg = String::new();
            let mut next: Option<&dyn StdError> = Some(&err);
            while let Some(err) = next {
                if !msg.is_empty() {
                    msg.push_str("\n  caused by: ");
                }
                msg.push_str(&format!("{}", err));
                next = err.source()
            }
            D::Error::custom(msg)
        })
    }
}

impl Serialize for Fragment {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        format!("{}", self).serialize(serializer)
    }
}

/// A DOM node in an HTML fragment. Note that we convert all character entities
/// to text nodes at parse time.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Node {
    /// A regular text node.
    #[non_exhaustive]
    Text {
        /// The text of this node.
        text: String,
    },

    /// An HTML element, with possible attributes.
    #[non_exhaustive]
    Element {
        /// The name of this element.
        name: String,

        /// HTML element attributes.
        attributes: Attributes,

        /// Child nodes.
        children: Vec<Node>,
    },
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO - Verify all strings are in legal ranges before printing.
        match *self {
            Node::Text { ref text } => {
                text.replace("&", "&amp;").replace("<", "&lt;").fmt(f)?;
            }
            Node::Element {
                ref name,
                ref attributes,
                ref children,
            } => {
                write!(f, "<{}", name)?;
                for (name, value) in attributes {
                    write!(
                        f,
                        " {}=\"{}\"",
                        name,
                        value.replace("&", "&amp;").replace("\"", "&quot;"),
                    )?;
                }
                write!(f, ">")?;
                if name != "br" && name != "img" {
                    for child in children {
                        child.fmt(f)?;
                    }
                    write!(f, "</{}>", name)?;
                }
            }
        }
        Ok(())
    }
}

/// HTML element attributes.
pub type Attributes = HashMap<String, String>;

fn parse_numeric_entity(
    digits: &str,
    radix: u32,
) -> result::Result<char, &'static str> {
    let code = u32::from_str_radix(digits, radix)
        .expect("parser should have required a valid number");
    if code == 0 {
        return Err("no \"\\0\" characters allowed in HTML");
    }
    ::std::char::from_u32(code).ok_or_else(|| "a valid UTF-8 character")
}

peg::parser! {
    grammar grammar() for str {
        use std::collections::HashMap;
        use std::iter::FromIterator;

        use super::{Fragment, Node, Attributes, parse_numeric_entity};

        pub(crate) rule fragment() -> Fragment
            = nodes:(node()*) {
                Fragment { nodes }
            }

        rule node() -> Node = text_node() / entity() / element()

        rule text_node() -> Node
            = quiet!{ text:$([^ '<' | '&']+) {
                Node::Text { text: text.to_owned() }
            } } / expected!("text")

        rule entity() -> Node
            = "&" text:( entity_named() / entity_hex() / entity_dec()) ";" {
                Node::Text { text }
            }

        rule entity_named() -> String
            = name:$("lt" / "gt" / "amp" / "apos" / "quot") {
                match name {
                    "lt" => "<".to_string(),
                    "gt" => ">".to_string(),
                    "amp" => "&".to_string(),
                    "apos" => "'".to_string(),
                    "quot" => "\"".to_string(),
                    _ => unreachable!("unknown named entity"),
                }
            }

        rule entity_hex() -> String
            = "#x" digits:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) {?
                parse_numeric_entity(digits, 16).map(|c| c.to_string())
            }

        rule entity_dec() -> String
            = "#" digits:$(['0'..='9']+) {?
                parse_numeric_entity(digits, 10).map(|c| c.to_string())
            }

        rule element() -> Node
            = empty_element() / start_end_element()

        rule empty_element() -> Node
            = "<" name:$(i("br") / i("img")) attributes:attributes() _* "/"? ">" {
                Node::Element {
                    name: name.to_owned(),
                    attributes,
                    children: vec![],
                }
            }

        rule start_end_element() -> Node
            = start_tag:start_tag() children:(node()*) end_tag:end_tag() {?
                let (name, attributes) = start_tag;
                if name == end_tag {
                    Ok(Node::Element {
                        name,
                        attributes,
                        children,
                    })
                } else {
                    Err("start and end tags must match")
                }
            }

        rule start_tag() -> (String, Attributes)
            = "<" name:$(name()) attributes:attributes() _* ">" {
                (name.to_owned(), attributes)
            }

        rule end_tag() -> String
            = quiet!{ "</" name:$(name()) _* ">" {
                name.to_owned()
            } } / expected!("end tag")

        rule attributes() -> Attributes
            = attributes:(attribute()*) {
                HashMap::from_iter(attributes)
            }

        rule attribute() -> (String, String)
            = quiet!{ _* name:$(name()) _* "=" _* att_value:att_value() {
                (name.to_owned(), att_value)
            } } / expected!("attribute")

        rule att_value() -> String
            = att_value_double_quotes() / att_value_single_quotes()

        // TODO: Handle escaped "<" and "&" values, if we ever need anything more
        // than "<font color=...>" support.
        rule att_value_double_quotes() -> String
            = "\"" text:$([^ '<' | '&' | '"']*) "\"" {
                text.to_owned()
            }

        // TODO: Handle escaped "<" and "&" values, if we ever need anything more
        // than "<font color=...>" support.
        rule att_value_single_quotes() -> String
            = "'" text:$([^ '<' | '&' |'\'']*) "'" {
                text.to_owned()
            }

        rule name() = quiet!{['A'..='Z' | 'a'..='z' | ':' | '_']['-' | 'A'..='Z' | 'a'..='z' | '0'..='9' | ':' | '_' | '.' ]*} / expected!("identifier")

        /// Case insensitive match.
        rule i(s: &'static str)
            = found:$([_]*<{s.len()}>) {?
                if found == s {
                    Ok(())
                } else {
                    Err(s)
                }
            }

        rule _ = quiet!{[' ' | '\r' | '\n' | '\t']+}
    }
}

#[test]
fn parse_valid_html_fragments() {
    let good_examples = &[
        "Hello!",
        "&amp;&lt;&gt;&quot;&apos;&#64;&#x4a;&#x4A;",
        "<b>foo</b><i>2</i><br><br >",
        "<font color=\"#444\">3</font><font color = 'blue' >4</font >",
    ];
    for example in good_examples {
        let _parsed: Fragment = example.parse().expect("failed to parse plain_text");
    }
}

#[test]
fn error_on_invalid_html_fragments() {
    let bad_examples = &[
        "<",
        "<foo",
        "a & b",
        "<b>",
        "<b></i>",
        "<font color></font>",
        "<font color=></font>",
        "<font color=\"></font>",
    ];
    for example in bad_examples {
        let result = example.parse::<Fragment>();
        match result {
            Ok(fragment) => {
                panic!(
                    "parsed {:?} as {:?}, but should have failed",
                    example, fragment,
                );
            }
            Err(err) => {
                println!(
                    "correctly got error: {}\n  caused by: {}",
                    err,
                    err.source().expect("error should have cause"),
                );
            }
        }
    }
}

#[test]
fn display_produces_canonical_string() {
    let examples = &[
        ("Hello!", "Hello!"),
        (
            "&amp;&lt;&gt;&quot;&apos;&#64;&#x4a;&#x4A;",
            "&amp;&lt;>\"'@JJ",
        ),
        ("<b>1</b><i>2</i><br><br >", "<b>1</b><i>2</i><br><br>"),
        (
            "<font color=\"#444\">3</font><font color = 'blue' >4</font >",
            "<font color=\"#444\">3</font><font color=\"blue\">4</font>",
        ),
    ];
    for &(input, expected) in examples {
        let fragment = input.parse::<Fragment>().expect("could not parse HTML");
        assert_eq!(format!("{}", fragment), expected);
    }

    let text_fragment = Fragment::from_text("A & B <");
    assert_eq!(format!("{}", text_fragment), "A &amp; B &lt;");
}
