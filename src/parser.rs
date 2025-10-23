use crate::{
    ast::{
        BinaryOp, Expr, ExprKind, FunctionParam, Literal, Module, Pattern, Stmt, StmtKind,
        TypeExpr, UnaryOp, WhenArm,
    },
    diagnostics::{Diagnostic, DiagnosticKind, SourceSpan},
    lexer::{Keyword, Lexer, Token, TokenKind},
};

pub fn parse_module(source: &str) -> Result<Module, Diagnostic> {
    let tokens = Lexer::new(source).tokenize()?;
    Parser::new(tokens).parse_module()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn parse_module(&mut self) -> Result<Module, Diagnostic> {
        let mut module_name: Option<Vec<String>> = None;
        let mut items = Vec::new();

        if self.matches_keyword(Keyword::Module) {
            let module_token = self.previous().clone();
            let (name, _span) = self.parse_module_path()?;
            if self.check(TokenKind::LBrace) {
                let _lbrace = self.advance();
                let body = self.parse_block_items(TokenKind::RBrace)?;
                let end = self.previous().span.end;
                items.push(Stmt {
                    span: SourceSpan {
                        start: module_token.span.start,
                        end,
                    },
                    kind: StmtKind::Module { name, items: body },
                });
            } else {
                module_name = Some(name);
                self.consume_optional_semicolon();
            }
        }

        while !self.check(TokenKind::Eof) {
            items.push(self.parse_statement()?);
        }

        Ok(Module {
            name: module_name,
            items,
        })
    }

    fn parse_module_path(&mut self) -> Result<(Vec<String>, SourceSpan), Diagnostic> {
        let name_token = self.consume_path_segment("expected module name")?;
        let start = name_token.span.start;
        let mut end = name_token.span.end;
        let mut segments = vec![name_token.lexeme.clone()];
        while self.matches(TokenKind::Dot) {
            let segment = self.consume_path_segment("expected module segment after `.`")?;
            end = segment.span.end;
            segments.push(segment.lexeme.clone());
        }
        Ok((segments, SourceSpan { start, end }))
    }

    fn parse_block_items(&mut self, terminator: TokenKind) -> Result<Vec<Stmt>, Diagnostic> {
        let mut items = Vec::new();
        while !self.check(terminator.clone()) && !self.check(TokenKind::Eof) {
            items.push(self.parse_statement()?);
        }
        self.consume(terminator, "expected block terminator")?;
        Ok(items)
    }

    fn parse_module_statement(&mut self) -> Result<Stmt, Diagnostic> {
        let module_token = self.consume_keyword(Keyword::Module)?;
        let (name, _) = self.parse_module_path()?;
        self.consume(TokenKind::LBrace, "expected `{` after module declaration")?;
        let items = self.parse_block_items(TokenKind::RBrace)?;
        let end = self.previous().span.end;
        Ok(Stmt {
            span: SourceSpan {
                start: module_token.span.start,
                end,
            },
            kind: StmtKind::Module { name, items },
        })
    }

    fn parse_use_statement(&mut self) -> Result<Stmt, Diagnostic> {
        let use_token = self.consume_keyword(Keyword::Use)?;
        let (path, span) = self.parse_module_path()?;
        let mut end = span.end;
        let alias = if self.matches_keyword(Keyword::As) {
            let alias_token = self.consume_identifier("expected alias after `as`")?;
            end = alias_token.span.end;
            Some(alias_token.lexeme.clone())
        } else {
            None
        };
        self.consume_optional_semicolon();
        Ok(Stmt {
            span: SourceSpan {
                start: use_token.span.start,
                end,
            },
            kind: StmtKind::Use { path, alias },
        })
    }

    fn parse_block(&mut self) -> Result<(Vec<Stmt>, SourceSpan), Diagnostic> {
        let lbrace = self.consume(TokenKind::LBrace, "expected `{` to start block")?;
        let start = lbrace.span.start;
        let items = self.parse_block_items(TokenKind::RBrace)?;
        let end = if let Some(last) = items.last() {
            last.span.end
        } else {
            self.previous().span.end
        };
        Ok((items, SourceSpan { start, end }))
    }

    fn parse_statement(&mut self) -> Result<Stmt, Diagnostic> {
        if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Keyword(Keyword::Var) => return self.parse_var_decl(false),
                TokenKind::Keyword(Keyword::Const) => return self.parse_const_decl(),
                TokenKind::Keyword(Keyword::Fn) => return self.parse_function(false),
                TokenKind::Keyword(Keyword::Module) => return self.parse_module_statement(),
                TokenKind::Keyword(Keyword::Use) => return self.parse_use_statement(),
                TokenKind::Keyword(Keyword::Async) => {
                    let async_token = self.advance();
                    let next = self
                        .peek()
                        .cloned()
                        .ok_or_else(|| self.error_eof("expected `fn` after `async`"))?;
                    if next.kind != TokenKind::Keyword(Keyword::Fn) {
                        return Err(self.error(&next, "expected `fn` after `async`"));
                    }
                    let mut fn_stmt = self.parse_function(true)?;
                    if let StmtKind::Function { is_async, .. } = &mut fn_stmt.kind {
                        *is_async = true;
                    }
                    fn_stmt.span.start = async_token.span.start;
                    return Ok(fn_stmt);
                }
                TokenKind::Keyword(Keyword::If) => return self.parse_if(),
                TokenKind::Keyword(Keyword::While) => return self.parse_while(),
                TokenKind::Keyword(Keyword::Loop) => return self.parse_loop(),
                TokenKind::Keyword(Keyword::For) => return self.parse_for(),
                TokenKind::Keyword(Keyword::When) => return self.parse_when(),
                TokenKind::Keyword(Keyword::Return) => return self.parse_return(),
                TokenKind::Keyword(Keyword::Break) => return self.parse_break(),
                TokenKind::Keyword(Keyword::Continue) => return self.parse_continue(),
                TokenKind::LBrace => {
                    let (items, span) = self.parse_block()?;
                    return Ok(Stmt {
                        kind: StmtKind::Block(items),
                        span,
                    });
                }
                _ => {}
            }
        }
        self.parse_expression_statement()
    }

    fn parse_var_decl(&mut self, is_mut: bool) -> Result<Stmt, Diagnostic> {
        let start = self.consume_keyword(Keyword::Var)?.span.start;
        if !is_mut {
            let _ = self.matches_keyword(Keyword::Mut);
        }
        let mutable = true;
        let name_token = self.consume_identifier("expected variable name")?;
        let annotation = if self.matches(TokenKind::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        let initializer = if self.matches(TokenKind::Assign) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume_optional_semicolon();
        let end = initializer
            .as_ref()
            .map(|expr| expr.span.end)
            .unwrap_or(name_token.span.end);
        Ok(Stmt {
            kind: StmtKind::VarDecl {
                name: name_token.lexeme.clone(),
                mutable,
                annotation,
                initializer,
            },
            span: SourceSpan { start, end },
        })
    }

    fn parse_const_decl(&mut self) -> Result<Stmt, Diagnostic> {
        let start = self.consume_keyword(Keyword::Const)?.span.start;
        let name_token = self.consume_identifier("expected constant name")?;
        let annotation = if self.matches(TokenKind::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        self.consume(TokenKind::Assign, "expected `=` in constant declaration")?;
        let value = self.parse_expression()?;
        self.consume_optional_semicolon();
        Ok(Stmt {
            span: SourceSpan {
                start,
                end: value.span.end,
            },
            kind: StmtKind::ConstDecl {
                name: name_token.lexeme.clone(),
                annotation,
                value,
            },
        })
    }

    fn parse_function(&mut self, is_async: bool) -> Result<Stmt, Diagnostic> {
        let start_token = self.consume_keyword(Keyword::Fn)?;
        let name_token = self.consume_identifier("expected function name")?;
        self.consume(TokenKind::LParen, "expected `(` after function name")?;
        let mut params = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                let param_name = self.consume_identifier("expected parameter name")?;
                let annotation = if self.matches(TokenKind::Colon) {
                    Some(self.parse_type_expr()?)
                } else {
                    None
                };
                params.push(FunctionParam {
                    name: param_name.lexeme.clone(),
                    annotation,
                    span: param_name.span,
                });
                if !self.matches(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenKind::RParen, "expected `)` after parameters")?;
        let return_type = if self.matches(TokenKind::Arrow) || self.matches(TokenKind::FatArrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        let (body, span) = self.parse_block()?;
        Ok(Stmt {
            span: SourceSpan {
                start: start_token.span.start,
                end: span.end,
            },
            kind: StmtKind::Function {
                name: name_token.lexeme.clone(),
                params,
                return_type,
                body,
                is_async,
            },
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, Diagnostic> {
        let start = self.consume_keyword(Keyword::If)?.span.start;
        let condition = self.parse_expression()?;
        let (then_branch, then_span) = self.parse_block()?;
        let else_branch = if self.matches_keyword(Keyword::Else) {
            if self.check(TokenKind::Keyword(Keyword::If)) {
                let else_stmt = self.parse_if()?;
                match else_stmt.kind {
                    StmtKind::Block(items) => Some(items),
                    _ => Some(vec![else_stmt]),
                }
            } else {
                let (branch, _) = self.parse_block()?;
                Some(branch)
            }
        } else {
            None
        };
        let end = else_branch
            .as_ref()
            .and_then(|branch| branch.last().map(|stmt| stmt.span.end))
            .unwrap_or(then_span.end);
        Ok(Stmt {
            span: SourceSpan { start, end },
            kind: StmtKind::If {
                condition,
                then_branch,
                else_branch,
            },
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, Diagnostic> {
        let start = self.consume_keyword(Keyword::While)?.span.start;
        let condition = self.parse_expression()?;
        let (body, span) = self.parse_block()?;
        Ok(Stmt {
            span: SourceSpan {
                start,
                end: span.end,
            },
            kind: StmtKind::While { condition, body },
        })
    }

    fn parse_loop(&mut self) -> Result<Stmt, Diagnostic> {
        let start = self.consume_keyword(Keyword::Loop)?.span.start;
        let (body, span) = self.parse_block()?;
        Ok(Stmt {
            span: SourceSpan {
                start,
                end: span.end,
            },
            kind: StmtKind::Loop { body },
        })
    }

    fn parse_for(&mut self) -> Result<Stmt, Diagnostic> {
        let start = self.consume_keyword(Keyword::For)?.span.start;
        let binding = self.consume_identifier("expected loop binding")?;
        self.consume_keyword(Keyword::In)?;
        let iterable = self.parse_expression()?;
        let (body, span) = self.parse_block()?;
        Ok(Stmt {
            span: SourceSpan {
                start,
                end: span.end,
            },
            kind: StmtKind::For {
                binding: binding.lexeme.clone(),
                iterable,
                body,
            },
        })
    }

    fn parse_when(&mut self) -> Result<Stmt, Diagnostic> {
        let start = self.consume_keyword(Keyword::When)?.span.start;
        let subject = self.parse_expression()?;
        let (arms, end) = self.parse_when_arms()?;
        Ok(Stmt {
            span: SourceSpan { start, end },
            kind: StmtKind::When { subject, arms },
        })
    }

    fn parse_when_arms(&mut self) -> Result<(Vec<WhenArm>, usize), Diagnostic> {
        self.consume(TokenKind::LBrace, "expected `{` after when expression")?;
        let mut arms = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            let pattern = if self.matches_keyword(Keyword::Else) {
                Pattern::Wildcard
            } else {
                self.parse_pattern()?
            };
            self.consume(TokenKind::Arrow, "expected `->` in when arm")?;
            let (body, span) = self.parse_block()?;
            arms.push(WhenArm {
                pattern,
                body,
                span,
            });
        }
        let end = self
            .consume(TokenKind::RBrace, "expected `}` after when arms")?
            .span
            .end;
        Ok((arms, end))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, Diagnostic> {
        if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Identifier => {
                    let ident = self.advance();
                    Ok(Pattern::Identifier(ident.lexeme.clone()))
                }
                TokenKind::LParen => {
                    self.advance();
                    let mut elements = Vec::new();
                    if !self.check(TokenKind::RParen) {
                        loop {
                            elements.push(self.parse_pattern()?);
                            if !self.matches(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    self.consume(TokenKind::RParen, "expected `)` to close tuple pattern")?;
                    Ok(Pattern::Tuple(elements))
                }
                TokenKind::Keyword(Keyword::True) => {
                    self.advance();
                    Ok(Pattern::Literal(Literal::Bool(true)))
                }
                TokenKind::Keyword(Keyword::False) => {
                    self.advance();
                    Ok(Pattern::Literal(Literal::Bool(false)))
                }
                TokenKind::Keyword(Keyword::None) => {
                    self.advance();
                    Ok(Pattern::Literal(Literal::None))
                }
                TokenKind::Number => {
                    let number = self.advance();
                    let literal = if number.lexeme.contains(['.', 'e', 'E']) {
                        Literal::Float(number.lexeme.parse().unwrap_or(0.0))
                    } else {
                        Literal::Int(number.lexeme.replace('_', "").parse().unwrap_or(0))
                    };
                    Ok(Pattern::Literal(literal))
                }
                TokenKind::String => {
                    let string = self.advance();
                    Ok(Pattern::Literal(Literal::String(string.lexeme.clone())))
                }
                _ => Err(self.error(token, "unexpected token in pattern")),
            }
        } else {
            Err(self.error_eof("unexpected end of input in pattern"))
        }
    }

    fn parse_return(&mut self) -> Result<Stmt, Diagnostic> {
        let token = self.consume_keyword(Keyword::Return)?;
        let expr = if self.check(TokenKind::Semicolon) || self.check(TokenKind::RBrace) {
            None
        } else if self.check(TokenKind::Eof) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume_optional_semicolon();
        let end = expr.as_ref().map(|e| e.span.end).unwrap_or(token.span.end);
        Ok(Stmt {
            span: SourceSpan {
                start: token.span.start,
                end,
            },
            kind: StmtKind::Return(expr),
        })
    }

    fn parse_break(&mut self) -> Result<Stmt, Diagnostic> {
        let token = self.consume_keyword(Keyword::Break)?;
        let expr = if self.check(TokenKind::Semicolon) || self.check(TokenKind::RBrace) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume_optional_semicolon();
        let end = expr.as_ref().map(|e| e.span.end).unwrap_or(token.span.end);
        Ok(Stmt {
            span: SourceSpan {
                start: token.span.start,
                end,
            },
            kind: StmtKind::Break(expr),
        })
    }

    fn parse_continue(&mut self) -> Result<Stmt, Diagnostic> {
        let token = self.consume_keyword(Keyword::Continue)?;
        self.consume_optional_semicolon();
        Ok(Stmt {
            span: token.span,
            kind: StmtKind::Continue,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, Diagnostic> {
        let expr = self.parse_expression()?;
        self.consume_optional_semicolon();
        Ok(Stmt {
            span: expr.span,
            kind: StmtKind::Expr(expr),
        })
    }

    fn parse_expression(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, Diagnostic> {
        let expr = self.parse_or()?;
        if self.matches(TokenKind::Assign) {
            let equals = self.previous().span;
            let value = self.parse_assignment()?;
            match expr.kind {
                ExprKind::Variable(_) | ExprKind::Index { .. } | ExprKind::Field { .. } => {
                    Ok(Expr {
                        span: SourceSpan {
                            start: expr.span.start,
                            end: value.span.end,
                        },
                        kind: ExprKind::Assign {
                            target: Box::new(expr),
                            value: Box::new(value),
                        },
                    })
                }
                _ => Err(
                    Diagnostic::new(DiagnosticKind::Parser, "invalid assignment target")
                        .with_span(equals),
                ),
            }
        } else {
            Ok(expr)
        }
    }

    fn parse_or(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_and()?;
        while self.matches(TokenKind::DoublePipe) {
            let right = self.parse_and()?;
            expr = Expr {
                span: SourceSpan {
                    start: expr.span.start,
                    end: right.span.end,
                },
                kind: ExprKind::Binary {
                    op: BinaryOp::Or,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_equality()?;
        while self.matches(TokenKind::DoubleAmpersand) {
            let right = self.parse_equality()?;
            expr = Expr {
                span: SourceSpan {
                    start: expr.span.start,
                    end: right.span.end,
                },
                kind: ExprKind::Binary {
                    op: BinaryOp::And,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.matches(TokenKind::EqualEqual) {
                let right = self.parse_comparison()?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: right.span.end,
                    },
                    kind: ExprKind::Binary {
                        op: BinaryOp::Equal,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                };
            } else if self.matches(TokenKind::BangEqual) {
                let right = self.parse_comparison()?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: right.span.end,
                    },
                    kind: ExprKind::Binary {
                        op: BinaryOp::NotEqual,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_term()?;
        while let Some(op) = if self.matches(TokenKind::LessEqual) {
            Some(BinaryOp::LessEqual)
        } else if self.matches(TokenKind::GreaterEqual) {
            Some(BinaryOp::GreaterEqual)
        } else if self.matches(TokenKind::Less) {
            Some(BinaryOp::Less)
        } else if self.matches(TokenKind::Greater) {
            Some(BinaryOp::Greater)
        } else {
            None
        } {
            let right = self.parse_term()?;
            expr = Expr {
                span: SourceSpan {
                    start: expr.span.start,
                    end: right.span.end,
                },
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_factor()?;
        loop {
            if self.matches(TokenKind::Plus) {
                let right = self.parse_factor()?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: right.span.end,
                    },
                    kind: ExprKind::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                };
            } else if self.matches(TokenKind::Minus) {
                let right = self.parse_factor()?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: right.span.end,
                    },
                    kind: ExprKind::Binary {
                        op: BinaryOp::Sub,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_unary()?;
        loop {
            if self.matches(TokenKind::Star) {
                let right = self.parse_unary()?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: right.span.end,
                    },
                    kind: ExprKind::Binary {
                        op: BinaryOp::Mul,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                };
            } else if self.matches(TokenKind::Slash) {
                let right = self.parse_unary()?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: right.span.end,
                    },
                    kind: ExprKind::Binary {
                        op: BinaryOp::Div,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                };
            } else if self.matches(TokenKind::Percent) {
                let right = self.parse_unary()?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: right.span.end,
                    },
                    kind: ExprKind::Binary {
                        op: BinaryOp::Mod,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, Diagnostic> {
        if self.matches(TokenKind::Minus) {
            let operator = self.previous().span;
            let right = self.parse_unary()?;
            Ok(Expr {
                span: SourceSpan {
                    start: operator.start,
                    end: right.span.end,
                },
                kind: ExprKind::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(right),
                },
            })
        } else if self.matches(TokenKind::Bang) {
            let operator = self.previous().span;
            let right = self.parse_unary()?;
            Ok(Expr {
                span: SourceSpan {
                    start: operator.start,
                    end: right.span.end,
                },
                kind: ExprKind::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(right),
                },
            })
        } else if self.matches_keyword(Keyword::Await) {
            let start = self.previous().span.start;
            let expr = self.parse_unary()?;
            Ok(Expr {
                span: SourceSpan {
                    start,
                    end: expr.span.end,
                },
                kind: ExprKind::Await(Box::new(expr)),
            })
        } else if self.matches_keyword(Keyword::Try) {
            let start = self.previous().span.start;
            let expr = self.parse_unary()?;
            Ok(Expr {
                span: SourceSpan {
                    start,
                    end: expr.span.end,
                },
                kind: ExprKind::Try(Box::new(expr)),
            })
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.matches(TokenKind::LParen) {
                let mut args = Vec::new();
                if !self.check(TokenKind::RParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.matches(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                let paren = self.consume(TokenKind::RParen, "expected `)` after arguments")?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: paren.span.end,
                    },
                    kind: ExprKind::Call {
                        callee: Box::new(expr),
                        args,
                    },
                };
            } else if self.matches(TokenKind::LBracket) {
                let index = self.parse_expression()?;
                let bracket = self.consume(TokenKind::RBracket, "expected `]` after index")?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: bracket.span.end,
                    },
                    kind: ExprKind::Index {
                        target: Box::new(expr),
                        index: Box::new(index),
                    },
                };
            } else if self.matches(TokenKind::Dot) {
                let ident = self.consume_identifier("expected field after `.`")?;
                expr = Expr {
                    span: SourceSpan {
                        start: expr.span.start,
                        end: ident.span.end,
                    },
                    kind: ExprKind::Field {
                        target: Box::new(expr),
                        field: ident.lexeme.clone(),
                    },
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, Diagnostic> {
        if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Keyword(Keyword::True) => {
                    let tok = self.advance();
                    Ok(Expr {
                        span: tok.span,
                        kind: ExprKind::Literal(Literal::Bool(true)),
                    })
                }
                TokenKind::Keyword(Keyword::False) => {
                    let tok = self.advance();
                    Ok(Expr {
                        span: tok.span,
                        kind: ExprKind::Literal(Literal::Bool(false)),
                    })
                }
                TokenKind::Keyword(Keyword::None) => {
                    let tok = self.advance();
                    Ok(Expr {
                        span: tok.span,
                        kind: ExprKind::Literal(Literal::None),
                    })
                }
                TokenKind::Keyword(Keyword::Some) => {
                    let tok = self.advance();
                    self.consume(TokenKind::LParen, "expected `(` after `some`")?;
                    let value = self.parse_expression()?;
                    let rparen = self.consume(TokenKind::RParen, "expected `)` after value")?;
                    Ok(Expr {
                        span: SourceSpan {
                            start: tok.span.start,
                            end: rparen.span.end,
                        },
                        kind: ExprKind::Call {
                            callee: Box::new(Expr {
                                span: tok.span,
                                kind: ExprKind::Variable("some".into()),
                            }),
                            args: vec![value],
                        },
                    })
                }
                TokenKind::Number => {
                    let tok = self.advance();
                    let literal = if tok.lexeme.contains(['.', 'e', 'E']) {
                        Literal::Float(tok.lexeme.replace('_', "").parse().unwrap_or(0.0))
                    } else {
                        Literal::Int(tok.lexeme.replace('_', "").parse().unwrap_or(0))
                    };
                    Ok(Expr {
                        span: tok.span,
                        kind: ExprKind::Literal(literal),
                    })
                }
                TokenKind::String => {
                    let tok = self.advance();
                    Ok(Expr {
                        span: tok.span,
                        kind: ExprKind::Literal(Literal::String(tok.lexeme.clone())),
                    })
                }
                TokenKind::Identifier => {
                    let tok = self.advance();
                    Ok(Expr {
                        span: tok.span,
                        kind: ExprKind::Variable(tok.lexeme.clone()),
                    })
                }
                TokenKind::LParen => {
                    let lparen = self.advance();
                    if self.matches(TokenKind::RParen) {
                        Ok(Expr {
                            span: SourceSpan {
                                start: lparen.span.start,
                                end: self.previous().span.end,
                            },
                            kind: ExprKind::TupleLiteral(Vec::new()),
                        })
                    } else {
                        let first = self.parse_expression()?;
                        if self.matches(TokenKind::Comma) {
                            let mut elems = vec![first];
                            while !self.check(TokenKind::RParen) && !self.check(TokenKind::Eof) {
                                elems.push(self.parse_expression()?);
                                if !self.matches(TokenKind::Comma) {
                                    break;
                                }
                            }
                            let rparen =
                                self.consume(TokenKind::RParen, "expected `)` after tuple")?;
                            Ok(Expr {
                                span: SourceSpan {
                                    start: lparen.span.start,
                                    end: rparen.span.end,
                                },
                                kind: ExprKind::TupleLiteral(elems),
                            })
                        } else {
                            let rparen =
                                self.consume(TokenKind::RParen, "expected `)` after expression")?;
                            Ok(Expr {
                                span: SourceSpan {
                                    start: lparen.span.start,
                                    end: rparen.span.end,
                                },
                                kind: ExprKind::Group(Box::new(first)),
                            })
                        }
                    }
                }
                TokenKind::LBracket => {
                    let lbracket = self.advance();
                    let mut elements = Vec::new();
                    if !self.check(TokenKind::RBracket) {
                        loop {
                            elements.push(self.parse_expression()?);
                            if !self.matches(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    let rbracket =
                        self.consume(TokenKind::RBracket, "expected `]` after array literal")?;
                    Ok(Expr {
                        span: SourceSpan {
                            start: lbracket.span.start,
                            end: rbracket.span.end,
                        },
                        kind: ExprKind::ArrayLiteral(elements),
                    })
                }
                TokenKind::LBrace => self.parse_inline_map(),
                TokenKind::Pipe => self.parse_lambda(),
                _ => Err(self.error(token, "unexpected token in expression")),
            }
        } else {
            Err(self.error_eof("unexpected end of expression"))
        }
    }

    fn parse_inline_map(&mut self) -> Result<Expr, Diagnostic> {
        let lbrace = self.advance();
        let mut entries = Vec::new();
        if !self.check(TokenKind::RBrace) {
            loop {
                let key = self.parse_expression()?;
                self.consume(TokenKind::Colon, "expected `:` in map literal")?;
                let value = self.parse_expression()?;
                entries.push((key, value));
                if !self.matches(TokenKind::Comma) {
                    break;
                }
            }
        }
        let rbrace = self.consume(TokenKind::RBrace, "expected `}` after map literal")?;
        Ok(Expr {
            span: SourceSpan {
                start: lbrace.span.start,
                end: rbrace.span.end,
            },
            kind: ExprKind::MapLiteral(entries),
        })
    }

    fn parse_lambda(&mut self) -> Result<Expr, Diagnostic> {
        let pipe = self.consume(TokenKind::Pipe, "expected `|` to start lambda")?;
        let mut params = Vec::new();
        if !self.check(TokenKind::Pipe) {
            loop {
                let param = self.consume_identifier("expected parameter in lambda")?;
                params.push(FunctionParam {
                    name: param.lexeme.clone(),
                    annotation: None,
                    span: param.span,
                });
                if !self.matches(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenKind::Pipe, "expected closing `|` in lambda")?;
        let body_expr = self.parse_expression()?;
        let body = vec![Stmt {
            span: body_expr.span,
            kind: StmtKind::Expr(body_expr.clone()),
        }];
        Ok(Expr {
            span: SourceSpan {
                start: pipe.span.start,
                end: body_expr.span.end,
            },
            kind: ExprKind::Lambda { params, body },
        })
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, Diagnostic> {
        let ident = self.consume_identifier("expected type name")?;
        let mut ty = TypeExpr {
            name: ident.lexeme.clone(),
            generics: Vec::new(),
        };
        if self.matches(TokenKind::Less) {
            let mut generics = Vec::new();
            loop {
                generics.push(self.parse_type_expr()?);
                if !self.matches(TokenKind::Comma) {
                    break;
                }
            }
            self.consume(TokenKind::Greater, "expected `>` to close generics")?;
            ty.generics = generics;
        }
        Ok(ty)
    }

    fn consume_optional_semicolon(&mut self) {
        let _ = self.matches(TokenKind::Semicolon);
    }

    fn matches(&mut self, kind: TokenKind) -> bool {
        if self.check(kind.clone()) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn matches_keyword(&mut self, keyword: Keyword) -> bool {
        if let Some(Token {
            kind: TokenKind::Keyword(k),
            ..
        }) = self.peek()
        {
            if *k == keyword {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<Token, Diagnostic> {
        if self.check(kind.clone()) {
            Ok(self.advance())
        } else {
            Err(self
                .peek()
                .map(|tok| self.error(tok, message))
                .unwrap_or_else(|| self.error_eof(message)))
        }
    }

    fn consume_keyword(&mut self, keyword: Keyword) -> Result<Token, Diagnostic> {
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Keyword(keyword.clone()) {
                Ok(self.advance())
            } else {
                Err(self.error(token, &format!("expected keyword `{keyword:?}`")))
            }
        } else {
            Err(self.error_eof("unexpected end of input"))
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<Token, Diagnostic> {
        if self.check(TokenKind::Identifier) {
            Ok(self.advance())
        } else {
            Err(self
                .peek()
                .map(|tok| self.error(tok, message))
                .unwrap_or_else(|| self.error_eof(message)))
        }
    }

    fn consume_path_segment(&mut self, message: &str) -> Result<Token, Diagnostic> {
        if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Identifier | TokenKind::Keyword(_) => Ok(self.advance()),
                _ => Err(self.error(token, message)),
            }
        } else {
            Err(self.error_eof(message))
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        if let Some(token) = self.peek() {
            token.kind == kind
        } else {
            false
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().clone()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Eof) | None)
    }

    fn error(&self, token: &Token, message: &str) -> Diagnostic {
        Diagnostic::new(DiagnosticKind::Parser, message.to_string()).with_span(token.span)
    }

    fn error_eof(&self, message: &str) -> Diagnostic {
        Diagnostic::new(DiagnosticKind::Parser, message.to_string())
    }
}
