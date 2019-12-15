use crate::loc::Loc;
use std::iter::Peekable;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Annot<T> {
    pub value: T,
    pub loc: Loc,
}

impl<T> Annot<T> {
    pub fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

#[derive(Debug)]
pub struct Lexer<I: Iterator<Item = char>> {
    loc: Loc,
    iter: Peekable<I>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(iter: I) -> Self {
        Lexer {
            loc: Loc::default(),
            iter: iter.peekable(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TokenKind {
    Symbol(String),
    Literal(String),
    Keyword(String),
    EndOfLine,
}

pub type Token = Annot<TokenKind>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LexError {
    InvalidCharacter(Annot<char>),
}

macro_rules! matches {
    ($e: expr, $($p: pat)|*) => { match $e { $($p)|* => true, _ => false } }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    fn peek(&mut self) -> Option<char> {
        self.iter.peek().copied()
    }

    fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    fn lex_comment(&mut self) -> Result<Option<Token>, LexError> {
        let c = self.next().unwrap();
        self.loc.proceed(c);
        loop {
            match self.peek() {
                Some('\x0a') | Some('\x0d') => {
                    return Ok(None);
                }
                Some(c) => {
                    self.next();
                    self.loc.proceed(c);
                }
                None => return Ok(None),
            }
        }
    }

    fn lex_whitespace(&mut self) -> Result<Option<Token>, LexError> {
        let c = self.next().unwrap();
        self.loc.proceed(c);
        loop {
            match self.peek() {
                Some(c) if c == ' ' || c == '\x09' => {
                    self.next();
                    self.loc.proceed(c);
                }
                _ => return Ok(None),
            }
        }
    }

    fn lex_line_end(&mut self) -> Result<Option<Token>, LexError> {
        let c = self.next().unwrap();
        let loc = self.loc;
        self.loc.proceed(c);
        if c == '\x0d' && self.peek() == Some('\x0a') {
            self.next();
        }
        Ok(Some(Token::new(TokenKind::EndOfLine, loc)))
    }

    fn lex_number_literal(&mut self) -> Result<Option<Token>, LexError> {
        let c = self.next().unwrap();
        let loc = self.loc;
        self.loc.proceed(c);
        let mut number = c.to_string();
        loop {
            match self.peek() {
                Some(c) if matches!(c, '0'..='9' | 'A'..='F' | 'H' | 'a'..='f' | 'h') => {
                    self.next();
                    self.loc.proceed(c);
                    number.push(c);
                }
                Some(c) if matches!(c, 'G'..='Z' | 'g'..='z' | '_') => {
                    return Err(LexError::InvalidCharacter(Annot::new(c, self.loc)))
                }
                _ => return Ok(Some(Token::new(TokenKind::Literal(number), loc))),
            }
        }
    }

    fn lex_keyword(&mut self) -> Result<Option<Token>, LexError> {
        let c = self.next().unwrap();
        let loc = self.loc;
        self.loc.proceed(c);
        let mut keyword = c.to_string();
        loop {
            match self.peek() {
                Some(c) if matches!(c, '0'..='9' | 'A'..='Z' | 'a'..='z' | '_' | '\x27') => {
                    self.next();
                    self.loc.proceed(c);
                    keyword.push(c);
                }
                _ => return Ok(Some(Token::new(TokenKind::Keyword(keyword), loc))),
            }
        }
    }

    fn lex_symbol(&mut self) -> Result<Option<Token>, LexError> {
        let c = self.next().unwrap();
        let loc = self.loc;
        self.loc.proceed(c);
        Ok(Some(Token::new(TokenKind::Symbol(c.to_string()), loc)))
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, LexError> {
        loop {
            let lexed = if let Some(c) = self.peek() {
                match c {
                    ';' => self.lex_comment(),
                    '0'..='9' => self.lex_number_literal(),
                    'A'..='Z' | 'a'..='z' | '_' => self.lex_keyword(),
                    ' ' | '\x09' => self.lex_whitespace(),
                    '\x0a' | '\x0d' => self.lex_line_end(),
                    '+' | '*' | '%' | '#' | '[' | ']' | '(' | ')' | '{' | '}' | ',' | '^' | '-'
                    | ':' | '.' | '<' | '>' | '&' | '|' | '!' => self.lex_symbol(),
                    _ => Err(LexError::InvalidCharacter(Annot::new(c, self.loc))),
                }
            } else {
                return Ok(None);
            };
            match lexed {
                Ok(None) => continue,
                _ => return lexed,
            }
        }
    }
}

#[test]
fn number_literal_test() {
    let mut lexer = Lexer::new("123h".chars());
    let token = lexer.next_token();
    assert_eq!(
        token.unwrap(),
        Some(Token::new(TokenKind::Literal("123h".to_owned()), Loc(0)))
    );
}

#[test]
fn keyword_test() {
    let mut lexer = Lexer::new("a123\n".chars());
    let token = lexer.next_token();
    assert_eq!(
        token.unwrap(),
        Some(Token::new(TokenKind::Keyword("a123".to_owned()), Loc(0)))
    );
}

#[test]
fn space_test() {
    let mut lexer = Lexer::new("sub b\n\r\nldi;x\r\tscf".chars());
    let expected = [
        (TokenKind::Keyword("sub".into()), Loc(0)),
        (TokenKind::Keyword("b".into()), Loc(0)),
        (TokenKind::EndOfLine, Loc(0)),
        (TokenKind::EndOfLine, Loc(1)),
        (TokenKind::Keyword("ldi".into()), Loc(2)),
        (TokenKind::EndOfLine, Loc(2)),
        (TokenKind::Keyword("scf".into()), Loc(3)),
    ];
    expected.iter().for_each(|(keyword, loc)| {
        assert_eq!(lexer.next_token().unwrap(), Some(Token::new(keyword.clone(), *loc)));
    });
    assert_eq!(lexer.next_token().unwrap(), None);
}
