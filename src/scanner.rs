use phf::phf_map;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use TokenType::*;

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => AND,
    "class" => CLASS,
    "else" => ELSE,
    "false" => FALSE,
    "for" => FOR,
    "fun" => FUN,
    "if" => IF,
    "nil" => NIL,
    "or" => OR,
    "print" => PRINT,
    "return" => RETURN,
    "super" => SUPER,
    "this" => THIS,
    "true" => TRUE,
    "var" => VAR,
    "while" => WHILE,
    "break" => BREAK,
    "continue" => CONTINUE,
};

#[derive(Debug)]
pub struct ScanError {
    line: usize,
    msg: String,
}

impl ScanError {
    fn new<T>(line: usize, msg: &str) -> Result<T, Self> {
        Err(Self {
            line,
            msg: msg.to_string(),
        })
    }
}

impl Display for ScanError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Syntax error: [line {} + ] Error {}",
            self.line, self.msg
        )
    }
}

impl Error for ScanError {}

// #[derive(Display)]
#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BangEqual,
    EQUAL,
    EqualEqual,
    GREATER,
    GreaterEqual,
    LESS,
    LessEqual,

    // Literals.
    IDENTIFIER,
    STRING(Literal),
    NUMBER(Literal),

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    BREAK,
    CONTINUE,
    EOF,
}

// #[derive(Display)]
#[derive(Debug, Clone)]
pub struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) lexeme: Arc<String>,
    pub(crate) line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(Arc<String>),
    Number(f64),
    NIL,
    Boolean(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(ref s) => write!(f, "{}", s),
            Self::Number(s) => write!(f, "{}", s),
            Self::NIL => write!(f, "null"),
            Self::Boolean(s) => write!(f, "{}", s),
        }
    }
}

pub struct Scanner {
    source: String,
    pub(crate) tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.to_owned(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.source.chars().nth(self.current);
        self.current += 1;
        ch
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, ScanError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.tokens.push(Token {
            token_type: EOF,
            lexeme: Arc::new("".to_owned()),
            line: self.line,
        });
        Ok(&self.tokens)
    }

    fn scan_token(&mut self) -> Result<(), ScanError> {
        let c = self.advance();
        if let Some(ch) = c {
            let token = match ch {
                '(' => Some(LeftParen),
                ')' => Some(RightParen),
                '{' => Some(LeftBrace),
                '}' => Some(RightBrace),
                ',' => Some(COMMA),
                '.' => Some(DOT),
                '-' => Some(MINUS),
                '+' => Some(PLUS),
                ';' => Some(SEMICOLON),
                '*' => Some(STAR),
                '!' => Some(if self.match_char('=') {
                    BangEqual
                } else {
                    BANG
                }),
                '=' => Some(if self.match_char('=') {
                    EqualEqual
                } else {
                    EQUAL
                }),
                '<' => Some(if self.match_char('=') {
                    LessEqual
                } else {
                    LESS
                }),
                '>' => Some(if self.match_char('=') {
                    GreaterEqual
                } else {
                    GREATER
                }),
                '/' => {
                    if self.match_char('/') {
                        while self.peek().filter(|&x| x != '\n').is_some() && !self.is_at_end() {
                            self.advance();
                        }
                        None
                    } else if self.match_char('*') {
                        while !self.is_at_end() {
                            match (self.peek(), self.peek_next()) {
                                (Some('*'), Some('/')) => break,
                                (Some('\n'), _) => {
                                    self.line += 1;
                                    self.advance();
                                }
                                _ => {
                                    self.advance();
                                }
                            }
                        }
                        self.advance();
                        self.advance();
                        None
                    } else {
                        Some(SLASH)
                    }
                }
                ' ' | '\r' | '\t' => None,
                '\n' => {
                    self.line += 1;
                    None
                }
                '"' => {
                    self.string()?;
                    None
                }
                ch => {
                    if ch.is_digit(10) {
                        self.number();
                    } else if ch.is_ascii_alphabetic() {
                        self.identifier();
                    } else {
                        ScanError::new(self.line, "Unexpected character.")?;
                    }
                    None
                }
            };
            if let Some(token) = token {
                self.add_token(token);
            }
        }
        Ok(())
    }

    fn identifier(&mut self) {
        loop {
            match self.peek() {
                Some(ch) if ch.is_ascii_alphanumeric() => {
                    self.advance();
                }
                _ => break,
            }
        }
        let txt = &self.source[self.start..self.current];
        let token_type = KEYWORDS.get(txt).unwrap_or(&IDENTIFIER);
        self.add_token(token_type.clone())
    }

    fn number(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }

        match (self.peek(), self.peek_next()) {
            (Some(ch), Some(next)) if ch == '.' && next.is_digit(10) => {
                self.advance();
                while let Some(ch) = self.peek() {
                    if !ch.is_digit(10) {
                        break;
                    }
                    self.advance();
                }
            }
            _ => {}
        }
        let val: f64 = self.source[self.start..self.current]
            .parse()
            .expect("Should be a valid number");
        self.add_token(NUMBER(Literal::Number(val)));
    }

    fn string(&mut self) -> Result<(), ScanError> {
        while let Some(ch) = self.peek() {
            if ch == '"' || self.is_at_end() {
                break;
            }
            if ch == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            ScanError::new(self.line, "Unterminated string.")?;
        }
        self.advance();
        let val = self.source[self.start + 1..self.current - 1].to_owned();
        self.add_token(STRING(Literal::String(Arc::new(val))));
        Ok(())
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
    }

    fn match_char(&mut self, expected_char: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if let Some(c) = self.peek() {
            if c != expected_char {
                return false;
            }
        }
        self.current += 1;
        true
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme = (&self.source[self.start..self.current]).to_owned();
        self.tokens.push(Token {
            token_type,
            lexeme: Arc::new(lexeme),
            line: self.line,
        })
    }
}
