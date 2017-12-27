//! A very simple version of an HTML data model. This is designed to correspond
//! to the limited HTML features supported in some subtitle formats, and to be
//! easy to sanitize using a whitelist of supported tags and attributes.

use failure::{self, ResultExt};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::Error as DeError;
use std::collections::HashMap;
use std::fmt;
use std::result;
use std::str::FromStr;

use super::Error;

/// Our custom HTML-lite grammar.
mod grammar {
    use std::result;

    /// Helper function to parse numeric character entities.
    fn parse_numeric_entity(
        digits: &str,
        radix: u32,
    ) -> result::Result<char, &'static str> {
        let code = u32::from_str_radix(digits, radix)
            .expect("parser should have required a valid number");
        if code == 0 {
            return Err("no \"\\0\" characters allowed in HTML");
        }
        ::std::char::from_u32(code).ok_or_else(|| {
            "a valid UTF-8 character"
        })
    }

    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

/// An HTML fragment.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Fragment {
    pub nodes: Vec<Node>
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
    type Err = failure::Error;

    fn from_str(html: &str) -> result::Result<Fragment, failure::Error> {
        Ok(grammar::fragment(html).with_context(|_| {
            Error::CouldNotParseHtml { html: html.to_owned() }
        })?)
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
        raw_html.parse().map_err(|err: failure::Error| {
            // Make sure we get the entire cause chain for this error.
            //
            // TODO: Use a writer to format this, or just use `common_failures`.
            let mut msg = String::new();
            for cause in err.causes() {
                if !msg.is_empty() {
                    msg.push_str("\n  caused by: ");
                }
                msg.push_str(&format!("{}", cause));
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
pub enum Node {
    /// A regular text node.
    Text { text: String },

    /// An HTML element, with possible attributes.
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
            Node::Element { ref name, ref attributes, ref children } => {
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

#[test]
fn parse_valid_html_fragments() {
    let good_examples = &[
        "Hello!",
        "&amp;&lt;&gt;&quot;&apos;&#64;&#x4a;&#x4A;",
        "<b>foo</b><i>2</i><br><br >",
        "<font color=\"#444\">3</font><font color = 'blue' >4</font >",
    ];
    for example in good_examples {
        let _parsed: Fragment = example.parse()
            .expect("failed to parse plain_text");
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
                    example,
                    fragment,
                );
            }
            Err(err) => {
                println!(
                    "correctly got error: {}\n  caused by: {}",
                    err,
                    err.cause().cause().expect("error should have cause"),
                );
            }
        }
    }
}

#[test]
fn display_produces_canonical_string() {
    let examples = &[
        ("Hello!", "Hello!"),
        ("&amp;&lt;&gt;&quot;&apos;&#64;&#x4a;&#x4A;", "&amp;&lt;>\"'@JJ"),
        ("<b>1</b><i>2</i><br><br >", "<b>1</b><i>2</i><br><br>"),
        ("<font color=\"#444\">3</font><font color = 'blue' >4</font >",
         "<font color=\"#444\">3</font><font color=\"blue\">4</font>"),
    ];
    for &(input, expected) in examples {
        let fragment = input.parse::<Fragment>()
            .expect("could not parse HTML");
        assert_eq!(format!("{}", fragment), expected);
    }

    let text_fragment = Fragment::from_text("A & B <");
    assert_eq!(format!("{}", text_fragment), "A &amp; B &lt;");
}
