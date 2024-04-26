use logos::Logos;

use crate::parser_utils::XkbFileParseError;

//below fn is part of scanner-utils.c
pub(crate) fn check_supported_char_encoding(s: &str) -> Result<&str, ()> {
    if s.len() < 2 {
        return Ok(s);
    }

    let bom = "\u{feff}";
    if let Some(s) = s.strip_prefix(bom) {
        return Ok(s);
    }

    // early detection of wrong file encoding, e.g. UTF-16 or UTF-32

    let mut c = s.chars();
    let first_char = c.next().unwrap();
    let second_char = c.next().unwrap();

    // TODO: is this necessary?
    if first_char == '\0' || second_char == '\0' {
        // TODO: handle case where first char not '\0'
        log::error!("Unexpected null character");
        return Err(());
    }

    Ok(s)
}

// TODO: return the span/location information
pub(crate) struct Lexer<'input> {
    token_stream: logos::SpannedIter<'input, RawToken>,
}

impl<'input> Lexer<'input> {
    pub(crate) fn new(input: &'input str) -> Result<Self, XkbFileParseError> {
        let input = check_supported_char_encoding(input)
            .map_err(|_| XkbFileParseError::WrongInputFormat)?;
        Ok(Self {
            token_stream: RawToken::lexer(input).spanned(),
        })
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(raw_token, _span)| match raw_token {
                Ok(raw_token) => Some(Token::from(raw_token)),
                Err(_) => None,
            })?
    }
}

#[allow(dead_code)]
#[derive(Logos, Debug, PartialEq)]
enum RawToken {
    #[regex("\"[^\"]*\"", |lex| lex.slice().parse().ok().map(process_string), priority=5)]
    String(String),

    #[regex(r"[[//]#][^\n]*[\n\r]?", |_| logos::Skip, priority=5)]
    Comment,

    // <is_graph*> but not <>
    // TODO: remove \x3B (semicolon)?
    #[regex(r"<[\x21-\x3B\x3D\x3F-\x7E]*>", 
        |lex| lex.slice().parse().ok().map(remove_brackets), priority=4)]
    Keyname(String),

    #[regex("[ \t\n]+", |_| logos::Skip, priority=3)]
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

    #[regex("[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice().parse().ok(), priority=2)]
    Ident(String),
    #[regex("0[xX][0-9a-fA-F]+", |lex| hex_convert(lex.slice().parse().ok()), priority=1)]
    HexNumber(u32),

    #[regex("[0-9]+", |lex| lex.slice().parse().ok(), priority=1)]
    UInt(u32),

    #[regex(r"[0-9]*\.[0-9]+", |lex| lex.slice().parse().ok(), priority=1)]
    Float(f64),
}

#[derive(Logos, Clone, Debug, PartialEq)]
pub(crate) enum Token {
    Comment,
    Whitespace,
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

impl From<RawToken> for Token {
    fn from(raw_token: RawToken) -> Self {
        match raw_token {
            RawToken::Comment => Token::Comment,
            RawToken::Whitespace => Token::Whitespace,
            RawToken::String(s) => Token::String(s),
            RawToken::Ident(s) => keyword_match(s),
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
            RawToken::Keyname(s) => Token::Keyname(s),
            RawToken::HexNumber(u) => Token::UInt(u),
        }
    }
}

fn hex_convert(token: Option<String>) -> Option<u32> {
    if let Some(token) = token {
        return u32::from_str_radix(&token[2..], 16).ok();
    }

    None
}

fn remove_brackets(token: String) -> String {
    let mut chars = token.chars();
    chars.next();
    chars.next_back();
    chars.collect()
}

fn process_string(token: String) -> String {
    // remove brackets
    let mut chars = token.chars();
    chars.next();
    chars.next_back();

    let mut string = String::new();

    // remove invalid escape sequences
    // backslash followed by one, two, or three
    // octal digits (0-7)
    while let Some(c) = chars.next() {
        if c == '\\' {
            let backslash = c;

            for i in 0..3 {
                if let Some(c) = chars.next() {
                    if ('0'..='7').contains(&c) {
                        // octal digit; skip
                        continue;
                    } else if i == 0 {
                        // TODO: does this work?
                        if ['n', 't', 'r', 'b', 'f', 'v'].contains(&c) {
                            // approved escape
                            string.push(backslash);
                            string.push(c);
                            break;
                        } else if i == 0 && c == 'e' {
                            // TODO: is this correct?
                            // octal \033 escape => \x1b
                            string += "\x1b";
                            break;
                        }
                    } else {
                        // TODO: warn unknown escape seq?
                        string.push(c);
                        break;
                    }
                }
            }
        } else {
            string.push(c);
        }
    }

    string
}

fn keyword_match(token: String) -> Token {
    use crate::text::lookup_key;
    match lookup_key(&crate::keywords::KEYWORDS, &token) {
        Some(keyword) => keyword.clone(),
        None => Token::Ident(token),
    }
}
