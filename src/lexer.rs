// based loosely on scanner.c
/*
 * Copyright © 2012 Ran Benita <ran234@gmail.com>
 * Copyright © 2024 wysiwys
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

pub(crate) use crate::lexer_utils::*;
use crate::parser_utils::XkbFileParseError;
use logos::Logos;
use std::iter::Peekable;

// TODO: return the span/location information
pub(crate) struct Lexer<'input> {
    bracket_depth: usize,
    closed_last_bracket: bool,
    finished_block: bool,
    token_stream: Peekable<logos::SpannedIter<'input, RawToken<'input>>>,
}

impl<'input> Lexer<'input> {
    pub(crate) fn new(input: &'input str) -> Result<Self, XkbFileParseError> {
        let input = check_supported_char_encoding(input)
            .map_err(|_| XkbFileParseError::WrongInputFormat)?;
        Ok(Self {
            bracket_depth: 0,
            closed_last_bracket: false,
            finished_block: false,
            token_stream: RawToken::lexer(input).spanned().peekable(),
        })
    }
    pub(crate) fn is_empty(&mut self) -> bool {
        self.token_stream.peek().is_none()
    }
    pub(crate) fn reset(&mut self) {
        self.bracket_depth = 0;
        self.closed_last_bracket = false;
        self.finished_block = false;
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // A bit hacky: `next` does not return an element if an XkbFile block is detected to have
        // ended. This is done here because lalrpop currently (does not seem to) support parsing
        // only part of the lexer's tokens, and then leaving off. However, we need to do this in
        // order to parse e.g. only the first of several XkbFiles in a file.

        if self.finished_block {
            return None;
        }
        self.token_stream
            .next()
            .map(|(raw_token, _span)| match raw_token {
                Ok(raw_token) => {
                    let token = Token::from(raw_token);

                    // Detect whether a block has ended.
                    // When the bracket_depth is lowered to 0 and followed by a semicolon,
                    // the block has ended.
                    // TODO: should the struct also track whether an Obrace preceded the last
                    // Cbrace?
                    if token == Token::Obrace {
                        self.bracket_depth += 1;
                    } else if token == Token::Cbrace {
                        if self.bracket_depth > 0 {
                            self.bracket_depth -= 1;
                        }
                        if self.bracket_depth == 0 {
                            self.closed_last_bracket = true;
                        }
                    } else if self.closed_last_bracket && token == Token::Semi {
                        self.finished_block = true;
                    }

                    Some(token)
                }
                Err(_) => None,
            })?
    }
}

#[allow(dead_code)]
#[derive(Logos, Debug, PartialEq)]
enum RawToken<'input> {
    #[regex("\"[^\"]*\"", priority = 5)]
    String(&'input str),

    #[regex(r"[[//]#][^\n]*[\n\r]?", |_| logos::Skip, priority=5)]
    Comment,

    // <is_graph*> but not <>
    #[regex(r"<[\x21-\x3B\x3D\x3F-\x7E]*>", priority = 4)]
    Keyname(&'input str),

    #[regex("[ \x00\t\n]+", |_| logos::Skip, priority=3)]
    Whitespace,

    #[token(";", priority = 3)]
    Semi,

    #[token(r"{", priority = 3)]
    Obrace,

    #[token(r"}", priority = 3)]
    Cbrace,

    #[token("=", priority = 3)]
    Equals,

    #[token(r"[", priority = 3)]
    Obracket,

    #[token(r"]", priority = 3)]
    Cbracket,

    #[token(r"(", priority = 3)]
    Oparen,

    #[token(r")", priority = 3)]
    Cparen,

    #[token(r".", priority = 3)]
    Dot,

    #[token(",", priority = 3)]
    Comma,

    #[token("+", priority = 3)]
    Plus,

    #[token(r"-", priority = 3)]
    Minus,

    #[token(r"*", priority = 3)]
    Times,

    #[token(r"/", priority = 3)]
    Divide,

    #[token(r"!", priority = 3)]
    Exclam,

    #[token(r"~", priority = 3)]
    Invert,

    #[regex("[A-Za-z_][A-Za-z0-9_]*", priority = 2)]
    Ident(&'input str),
    #[regex("0[xX][0-9a-fA-F]+", |lex| hex_convert(lex.slice()), priority=1)]
    HexNumber(u32),

    #[regex("[0-9]+", |lex| lex.slice().parse().ok(), priority=1)]
    UInt(u32),

    #[regex(r"[0-9]*\.[0-9]+", |lex| lex.slice().parse().ok(), priority=1)]
    Float(f64),
}
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Token {
    Skip,
    Keyname(String),
    String(String),
    Ident(String),
    UInt(u32),
    Float(f64),
    Semi,
    Obrace,
    Cbrace,
    Equals,
    Obracket,
    Cbracket,
    Oparen,
    Cparen,
    Dot,
    Comma,
    Plus,
    Minus,
    Times,
    Divide,
    Exclam,
    Invert,
    ActionTok,
    Alias,
    AlphanumericKeys,
    AlternateGroup,
    Alternate,
    Augment,
    Default,
    FunctionKeys,
    Group,
    Hidden,
    Include,
    Indicator,
    Interpret,
    KeypadKeys,
    Key,
    Keys,
    Logo,
    ModifierKeys,
    ModifierMap,
    Outline,
    Overlay,
    Override,
    Partial,
    Replace,
    Row,
    Section,
    Shape,
    Solid,
    Text,
    Type,
    VirtualMods,
    Virtual,
    XkbCompatmap,
    XkbGeometry,
    XkbKeycodes,
    XkbKeymap,
    XkbLayout,
    XkbSemantics,
    XkbSymbols,
    XkbTypes,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'token> From<RawToken<'token>> for Token {
    fn from(raw_token: RawToken) -> Self {
        match raw_token {
            RawToken::Comment => Token::Skip,
            RawToken::Whitespace => Token::Skip,
            // remove brackets and process escape codes
            RawToken::String(s) => Token::String(process_string(s[1..s.len() - 1].as_bytes())),
            RawToken::Ident(s) => Token::keyword_match(s),
            RawToken::UInt(s) => Token::UInt(s),
            RawToken::Float(f) => Token::Float(f),
            RawToken::Semi => Token::Semi,
            RawToken::Obrace => Token::Obrace,
            RawToken::Cbrace => Token::Cbrace,
            RawToken::Equals => Token::Equals,
            RawToken::Obracket => Token::Obracket,
            RawToken::Cbracket => Token::Cbracket,
            RawToken::Oparen => Token::Oparen,
            RawToken::Cparen => Token::Cparen,
            RawToken::Dot => Token::Dot,
            RawToken::Comma => Token::Comma,
            RawToken::Plus => Token::Plus,
            RawToken::Minus => Token::Minus,
            RawToken::Times => Token::Times,
            RawToken::Divide => Token::Divide,
            RawToken::Exclam => Token::Exclam,
            RawToken::Invert => Token::Invert,
            // remove brackets
            RawToken::Keyname(s) => Token::Keyname(s[1..s.len() - 1].into()),
            RawToken::HexNumber(u) => Token::UInt(u),
        }
    }
}

fn hex_convert(token: &str) -> Option<u32> {
    u32::from_str_radix(&token[2..], 16).ok()
}

// based on string processing part of _xkbcommon_lex
// assumes outer quotes have been removed
fn process_string(bytes: &[u8]) -> String {
    let len = bytes.len();
    let mut new: Vec<u8> = Vec::with_capacity(len);
    let mut i = 0;

    while i < len {
        if let Some(esc) = bytes.get(i..i + 2) {
            let mut increment = 2;

            let backslash: u8 = '\\'.try_into().unwrap();
            match esc {
                s if s.starts_with(&[backslash]) => {
                    match s[1] as char {
                        'n' => new.extend("\n".as_bytes()),
                        't' => new.extend("\t".as_bytes()),
                        'r' => new.extend("\r".as_bytes()),
                        'b' => new.extend("\\".as_bytes()), //backslash
                        'f' => new.extend("\x0c".as_bytes()), // form feed page break
                        'v' => new.extend("\x0b".as_bytes()),
                        'e' => new.extend("\x1b".as_bytes()), // octal \033
                        _ => {
                            // get the next 1..3 characters.
                            let octal = bytes
                                .get(i + 1..i + 4)
                                .or_else(|| bytes.get(i + 1..i + 3))
                                .or_else(|| s.get(1..2))
                                .unwrap()
                                .iter()
                                .map(|byte| *byte as char)
                                .take_while(|c| ('0'..='7').contains(c))
                                .collect::<String>();

                            if !octal.is_empty() {
                                if let Ok(c) = u8::from_str_radix(&octal, 8) {
                                    // skip \0, \00, \000
                                    if c != 0 {
                                        new.push(c);
                                    }
                                }
                                increment += octal.len() - 1;
                            }
                        }
                    }
                }
                // non-escape
                s => {
                    new.push(s[0]);
                    increment = 1;
                }
            };
            i += increment;
        } else {
            assert_eq!(i + 1, len);

            new.push(bytes[i]);
            i += 1;
        }
    }

    String::from_utf8(new).expect("escaped string is not valid utf8")
}

impl Token {
    fn keyword_match(token: &str) -> Self {
        crate::text::lookup_key(&crate::keywords::KEYWORDS, token)
            .cloned()
            .unwrap_or_else(|| Token::Ident(token.into()))
    }
}

#[cfg(test)]
mod test {

    fn test_process_string(s: &str) -> String {
        process_string(&s[1..s.len() - 1].as_bytes())
    }
    use super::*;
    #[test]
    fn test_string_process() {
        assert_eq!(test_process_string(r#""""#), "");
        assert_eq!(test_process_string(r#""Test\e""#), "Test\x1b");
        assert_eq!(test_process_string(r#""Test\e1""#), "Test\x1b1");
        assert_eq!(test_process_string(r#""Test\00f""#), "Testf");
        assert_eq!(test_process_string(r#""Test\00\00\0f""#), "Testf");
        assert_eq!(test_process_string(r#""\456Test\00\00\082""#), "Test82");
        assert_eq!(test_process_string(r#""\456\00\00\081""#), "81");
        assert_eq!(test_process_string(r#""\000\00\0000\00\""#), r"0\");
        assert_eq!(test_process_string(r#""\000\00\000\00\""#), r"\");
        assert_eq!(test_process_string(r#""\000\00\0\00""#), r"");
        assert_eq!(test_process_string(r#""\456Test\0000""#), "Test0");
        assert_eq!(test_process_string(r#""Test\9f""#), "Testf");
        assert_eq!(test_process_string(r#""Test\1f""#), "Test\u{1}f");
        assert_eq!(test_process_string(r#""Test\1\2""#), "Test\u{1}\u{2}");
        assert_eq!(test_process_string(r#""Test\401\2""#), "Test\u{2}");
    }
}
