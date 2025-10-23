use crate::diagnostics::{Diagnostic, DiagnosticKind, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Var,
    Mut,
    Const,
    Fn,
    Async,
    Module,
    Use,
    Pub,
    If,
    Else,
    When,
    Loop,
    While,
    For,
    In,
    Break,
    Continue,
    Return,
    True,
    False,
    None,
    Some,
    Try,
    Await,
    Struct,
    Enum,
    Trait,
    Impl,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Number,
    String,
    Keyword(Keyword),
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
    Arrow,
    FatArrow,
    ThinArrow,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    DoubleAmpersand,
    DoublePipe,
    Bang,
    BangEqual,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Unknown,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: SourceSpan,
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: std::str::CharIndices<'a>,
    current: usize,
    peeked: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices(),
            current: 0,
            peeked: None,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        let next = if let Some((idx, ch)) = self.peeked.take() {
            Some((idx, ch))
        } else {
            self.chars.next()
        };
        if let Some((idx, ch)) = next {
            self.current = idx + ch.len_utf8();
            Some((idx, ch))
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        if self.peeked.is_none() {
            self.peeked = self.chars.next();
        }
        self.peeked
    }

    fn match_next(&mut self, expected: char) -> bool {
        if let Some((idx, ch)) = self.peek() {
            if ch == expected {
                self.peeked = None;
                self.current = idx + ch.len_utf8();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn collect_while<F>(&mut self, start: usize, mut predicate: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let mut end = self.current;
        while let Some((idx, ch)) = self.peek() {
            if predicate(ch) {
                self.bump();
                end = idx + ch.len_utf8();
            } else {
                break;
            }
        }
        self.source[start..end].to_string()
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            let mut progressed = false;

            while let Some((_, ch)) = self.peek() {
                if ch.is_whitespace() {
                    self.bump();
                    progressed = true;
                } else {
                    break;
                }
            }

            let mut handled_comment = false;
            if let Some((start, '/')) = self.peek() {
                if let Some((_, next)) = self.chars.clone().next() {
                    if next == '/' {
                        self.bump();
                        self.bump();
                        while let Some((_, ch)) = self.peek() {
                            if ch == '\n' {
                                break;
                            }
                            self.bump();
                        }
                        handled_comment = true;
                    } else if next == '*' {
                        self.bump();
                        self.bump();
                        let mut depth = 1;
                        while let Some((_, ch)) = self.bump() {
                            if ch == '/' {
                                if let Some((_, '*')) = self.peek() {
                                    self.bump();
                                    depth += 1;
                                }
                            } else if ch == '*' {
                                if let Some((_, '/')) = self.peek() {
                                    self.bump();
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                            }
                        }
                        handled_comment = true;
                    }
                }
                if !handled_comment {
                    self.peeked = Some((start, '/'));
                }
            }

            if handled_comment {
                progressed = true;
            }

            if !progressed {
                break;
            }
        }
    }

    fn identifier_or_keyword(&mut self, start: usize) -> Token {
        self.collect_while(start, |ch| ch.is_alphanumeric() || ch == '_');
        let end = self.current;
        let lexeme = self.source[start..end].to_string();
        let kind = keyword_for(&lexeme).unwrap_or(TokenKind::Identifier);
        Token {
            kind,
            lexeme,
            span: SourceSpan { start, end },
        }
    }

    fn number_literal(&mut self, start: usize) -> Token {
        let mut end = self.current;
        let mut seen_dot = false;
        while let Some((idx, ch)) = self.peek() {
            match ch {
                '0'..='9' | '_' => {
                    self.bump();
                    end = idx + ch.len_utf8();
                }
                '.' if !seen_dot => {
                    seen_dot = true;
                    self.bump();
                    end = idx + 1;
                }
                'e' | 'E' => {
                    self.bump();
                    end = idx + 1;
                    if let Some((_, sign @ ('+' | '-'))) = self.peek() {
                        self.bump();
                        end += sign.len_utf8();
                    }
                }
                _ => break,
            }
        }
        let lexeme = self.source[start..end].to_string();
        Token {
            kind: TokenKind::Number,
            lexeme,
            span: SourceSpan { start, end },
        }
    }

    fn string_literal(&mut self, start: usize) -> Result<Token, Diagnostic> {
        let mut end = self.current;
        let mut value = String::new();
        while let Some((idx, ch)) = self.bump() {
            end = idx + ch.len_utf8();
            match ch {
                '"' => {
                    return Ok(Token {
                        kind: TokenKind::String,
                        lexeme: value,
                        span: SourceSpan { start, end },
                    });
                }
                '\\' => {
                    if let Some((_, esc)) = self.bump() {
                        end = idx + 1 + esc.len_utf8();
                        match esc {
                            'n' => value.push('\n'),
                            'r' => value.push('\r'),
                            't' => value.push('\t'),
                            '"' => value.push('"'),
                            '\\' => value.push('\\'),
                            other => value.push(other),
                        }
                    } else {
                        break;
                    }
                }
                _ => value.push(ch),
            }
        }
        Err(
            Diagnostic::new(DiagnosticKind::Lexer, "unterminated string literal")
                .with_span(SourceSpan { start, end }),
        )
    }

    fn simple_token(&mut self, start: usize, kind: TokenKind) -> Token {
        let end = self.current;
        Token {
            kind,
            lexeme: self.source[start..end].to_string(),
            span: SourceSpan { start, end },
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, Diagnostic> {
        let mut tokens = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            let (start, ch) = match self.bump() {
                Some(pair) => pair,
                None => {
                    tokens.push(Token {
                        kind: TokenKind::Eof,
                        lexeme: String::new(),
                        span: SourceSpan {
                            start: self.current,
                            end: self.current,
                        },
                    });
                    break;
                }
            };

            let token = match ch {
                'a'..='z' | 'A'..='Z' | '_' => self.identifier_or_keyword(start),
                '0'..='9' => self.number_literal(start),
                '"' => self.string_literal(start)?,
                '(' => self.simple_token(start, TokenKind::LParen),
                ')' => self.simple_token(start, TokenKind::RParen),
                '{' => self.simple_token(start, TokenKind::LBrace),
                '}' => self.simple_token(start, TokenKind::RBrace),
                '[' => self.simple_token(start, TokenKind::LBracket),
                ']' => self.simple_token(start, TokenKind::RBracket),
                ',' => self.simple_token(start, TokenKind::Comma),
                '.' => {
                    if self.match_next('.') {
                        self.simple_token(start, TokenKind::ThinArrow)
                    } else {
                        self.simple_token(start, TokenKind::Dot)
                    }
                }
                ';' => self.simple_token(start, TokenKind::Semicolon),
                ':' => self.simple_token(start, TokenKind::Colon),
                '+' => self.simple_token(start, TokenKind::Plus),
                '-' => {
                    if self.match_next('>') {
                        self.simple_token(start, TokenKind::Arrow)
                    } else {
                        self.simple_token(start, TokenKind::Minus)
                    }
                }
                '*' => self.simple_token(start, TokenKind::Star),
                '%' => self.simple_token(start, TokenKind::Percent),
                '=' => {
                    if self.match_next('>') {
                        self.simple_token(start, TokenKind::FatArrow)
                    } else if self.match_next('=') {
                        self.simple_token(start, TokenKind::EqualEqual)
                    } else {
                        self.simple_token(start, TokenKind::Assign)
                    }
                }
                '!' => {
                    if self.match_next('=') {
                        self.simple_token(start, TokenKind::BangEqual)
                    } else {
                        self.simple_token(start, TokenKind::Bang)
                    }
                }
                '&' => {
                    if self.match_next('&') {
                        self.simple_token(start, TokenKind::DoubleAmpersand)
                    } else {
                        self.simple_token(start, TokenKind::Ampersand)
                    }
                }
                '|' => {
                    if self.match_next('|') {
                        self.simple_token(start, TokenKind::DoublePipe)
                    } else {
                        self.simple_token(start, TokenKind::Pipe)
                    }
                }
                '<' => {
                    if self.match_next('=') {
                        self.simple_token(start, TokenKind::LessEqual)
                    } else {
                        self.simple_token(start, TokenKind::Less)
                    }
                }
                '>' => {
                    if self.match_next('=') {
                        self.simple_token(start, TokenKind::GreaterEqual)
                    } else {
                        self.simple_token(start, TokenKind::Greater)
                    }
                }
                '/' => self.simple_token(start, TokenKind::Slash),
                _ => self.simple_token(start, TokenKind::Unknown),
            };
            tokens.push(token);
        }
        Ok(tokens)
    }
}

fn keyword_for(ident: &str) -> Option<TokenKind> {
    use self::Keyword as Kw;
    let keyword = match ident {
        "var" => Kw::Var,
        "mut" => Kw::Mut,
        "const" => Kw::Const,
        "fn" => Kw::Fn,
        "async" => Kw::Async,
        "module" => Kw::Module,
        "use" => Kw::Use,
        "pub" => Kw::Pub,
        "if" => Kw::If,
        "else" => Kw::Else,
        "when" => Kw::When,
        "loop" => Kw::Loop,
        "while" => Kw::While,
        "for" => Kw::For,
        "in" => Kw::In,
        "break" => Kw::Break,
        "continue" => Kw::Continue,
        "return" => Kw::Return,
        "true" => Kw::True,
        "false" => Kw::False,
        "none" => Kw::None,
        "some" => Kw::Some,
        "try" => Kw::Try,
        "await" => Kw::Await,
        "struct" => Kw::Struct,
        "enum" => Kw::Enum,
        "trait" => Kw::Trait,
        "impl" => Kw::Impl,
        _ => return None,
    };
    Some(TokenKind::Keyword(keyword))
}
