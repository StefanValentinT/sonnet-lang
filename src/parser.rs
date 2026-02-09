use crate::ast::ast_type::Const;
use crate::ast::untyped_ast::*;
use crate::{
    ast::ast_type::*,
    lexer::Token,
    queue::{IsQueue, Queue},
};

pub fn parse(tokens: Queue<Token>) -> Program {
    let mut tokens = tokens;
    let mut funcs = Vec::new();

    while tokens.peek().unwrap() != Token::EOF {
        funcs.push(parse_fun_decl(&mut tokens));
    }

    Program::Program(funcs)
}

fn parse_fun_decl(tokens: &mut Queue<Token>) -> FunDecl {
    let extern_fun = if tokens.peek().unwrap() == Token::Keyword("extern".to_string()) {
        tokens.consume();
        true
    } else {
        false
    };
    let exec_time = match tokens.remove().unwrap() {
        Token::Keyword(ref s) if s == "fun" => ExecTime::Runtime,
        Token::Keyword(ref s) if s == "cofun" => ExecTime::CompileTime,
        v => panic!(
            "Expected fun or cofun keyword at start of of function declaration, but got {:#?}",
            v
        ),
    };

    let name = match tokens.remove().unwrap() {
        Token::Identifier(n) => n,
        t => panic!("Expected function name, got {:?}", t),
    };

    expect(Token::OpenParen, tokens);
    let params = parse_param_list(tokens);
    expect(Token::CloseParen, tokens);

    let ret_type = match tokens.peek().unwrap() {
        Token::Semicolon | Token::OpenBrace => Type::Unit,
        _ => parse_type(tokens),
    };

    let body = match tokens.peek().unwrap() {
        Token::OpenBrace => {
            if extern_fun {
                panic!("Extern function has a body.")
            }
            Some(parse_block(tokens))
        }
        Token::Semicolon => {
            tokens.consume();
            if !extern_fun {
                panic!("Not extern function doesn't have a body.")
            }
            None
        }
        t => panic!("Expected {{ or ; after function declaration, got {:?}", t),
    };

    FunDecl {
        name,
        params,
        body,
        ret_type,
        exec_time,
    }
}

fn parse_param_list(tokens: &mut Queue<Token>) -> Vec<(String, Type)> {
    match tokens.peek().unwrap() {
        Token::CloseParen => Vec::new(),
        _ => {
            let mut params = Vec::new();
            loop {
                let name = match tokens.remove().unwrap() {
                    Token::Identifier(n) => n,
                    t => panic!("Expected parameter name, got {:?}", t),
                };
                expect(Token::Colon, tokens);

                let ty = parse_type(tokens);

                params.push((name, ty));

                if tokens.peek().unwrap() == Token::Comma {
                    tokens.consume();
                } else {
                    break;
                }
            }
            params
        }
    }
}

fn parse_type(tokens: &mut Queue<Token>) -> Type {
    match tokens.remove().unwrap() {
        Token::Star => {
            let inner = parse_type(tokens);
            Type::Pointer {
                referenced: Box::new(inner),
            }
        }
        Token::Keyword(ref s) if s == "I32" => Type::I32,
        Token::Keyword(ref s) if s == "I64" => Type::I64,
        Token::Keyword(ref s) if s == "F64" => Type::F64,
        Token::Keyword(ref s) if s == "Unit" => Type::Unit,
        Token::Keyword(ref s) if s == "Char" => Type::Char,
        Token::OpenBracket => {
            let element_type = match tokens.remove().unwrap() {
                Token::Keyword(ref s) if s == "I32" => Type::I32,
                Token::Keyword(ref s) if s == "I64" => Type::I64,
                Token::Keyword(ref s) if s == "F64" => Type::F64,
                Token::Keyword(ref s) if s == "Char" => Type::Char,
                Token::Star => {
                    let inner = parse_type(tokens);
                    Type::Pointer {
                        referenced: Box::new(inner),
                    }
                }
                t => panic!("Expected type inside array, got {:?}", t),
            };

            match tokens.peek().unwrap() {
                Token::Semicolon => {
                    tokens.consume();
                    let len = match tokens.remove().unwrap() {
                        Token::IntLiteral32(v) => v as usize,
                        Token::IntLiteral64(v) => v as usize,
                        t => panic!("Expected array length, got {:?}", t),
                    };
                    expect(Token::CloseBracket, tokens);

                    Type::Array {
                        element_type: Box::new(element_type),
                        size: len as i32,
                    }
                }
                Token::CloseBracket => {
                    tokens.consume();
                    Type::Slice {
                        element_type: Box::new(element_type),
                    }
                }
                t => panic!("Expected ; or ] after array type, got {:?}", t),
            }
        }
        t => panic!("Expected type, got {:?}", t),
    }
}

fn parse_block(tokens: &mut Queue<Token>) -> Block {
    expect(Token::OpenBrace, tokens);

    let mut items = Vec::new();

    loop {
        match tokens.peek().unwrap() {
            Token::CloseBrace => {
                tokens.consume();
                return Block::Block(
                    items,
                    Expr {
                        ty: Some(Type::Unit),
                        kind: ExprKind::Constant(Const::Unit),
                    },
                );
            }

            Token::Keyword(ref s) if s == "let" => {
                items.push(BlockItem::D(parse_declaration(tokens)));
            }

            _ => {
                let expr = parse_expr(tokens, 0);

                if tokens.peek().unwrap() == Token::Semicolon {
                    tokens.consume();
                    items.push(BlockItem::S(Stmt::Expression(expr)));
                } else {
                    expect(Token::CloseBrace, tokens);
                    return Block::Block(items, expr);
                }
            }
        }
    }
}

fn parse_declaration(tokens: &mut Queue<Token>) -> Decl {
    expect(Token::Keyword("let".into()), tokens);

    let name = match tokens.remove().unwrap() {
        Token::Identifier(n) => n,
        t => panic!("Expected variable name, got {:?}", t),
    };

    expect(Token::Colon, tokens);
    let var_type = parse_type(tokens);

    expect(Token::Assign, tokens);
    let mut init_expr = parse_expr(tokens, 0);
    init_expr.ty = Some(var_type.clone());
    let initializer = Initializer::InitExpr(init_expr);

    expect(Token::Semicolon, tokens);

    Decl::Variable(VarDecl {
        name,
        initializer,
        var_type,
    })
}

fn parse_statement(tokens: &mut Queue<Token>) -> Stmt {
    match tokens.peek().unwrap() {
        Token::Keyword(ref s) => match s.as_str() {
            "break" => {
                tokens.consume();
                expect(Token::Semicolon, tokens);
                Stmt::Break {
                    label: String::new(),
                }
            }

            "continue" => {
                tokens.consume();
                expect(Token::Semicolon, tokens);
                Stmt::Continue {
                    label: String::new(),
                }
            }

            "while" => {
                tokens.consume();
                expect(Token::OpenParen, tokens);
                let cond = parse_expr(tokens, 0);
                expect(Token::CloseParen, tokens);
                let body = Box::new(parse_statement(tokens));
                Stmt::While {
                    condition: cond,
                    body,
                    label: String::new(),
                }
            }

            _ => {
                let expr = parse_expr(tokens, 0);
                expect(Token::Semicolon, tokens);
                Stmt::Expression(expr)
            }
        },

        Token::Semicolon => {
            tokens.consume();
            Stmt::Null
        }

        _ => {
            let expr = parse_expr(tokens, 0);
            expect(Token::Semicolon, tokens);
            Stmt::Expression(expr)
        }
    }
}

fn parse_factor(tokens: &mut Queue<Token>) -> Expr {
    match tokens.remove().unwrap() {
        Token::Keyword(ref s) if s == "if" => {
            let cond = parse_expr(tokens, 0);
            expect(Token::Keyword("then".to_string()), tokens);
            let then_expr = parse_expr(tokens, 0);
            expect(Token::Keyword("else".to_string()), tokens);
            let else_expr = parse_expr(tokens, 0);
            let ty = then_expr.ty.clone();
            Expr {
                ty,
                kind: ExprKind::IfThenElse(
                    Box::new(cond),
                    Box::new(then_expr),
                    Box::new(else_expr),
                ),
            }
        }

        Token::Ampersand => {
            let inner = parse_factor(tokens);
            Expr {
                ty: None,
                kind: ExprKind::AddrOf(Box::new(inner)),
            }
        }

        Token::Star => {
            let inner = parse_factor(tokens);
            Expr {
                ty: None,
                kind: ExprKind::Dereference(Box::new(inner)),
            }
        }

        Token::OpenBracket => {
            let mut elements = Vec::new();

            if tokens.peek().unwrap() != Token::CloseBracket {
                loop {
                    elements.push(parse_expr(tokens, 0));
                    if tokens.peek().unwrap() == Token::Comma {
                        tokens.consume();
                    } else {
                        break;
                    }
                }
            }

            expect(Token::CloseBracket, tokens);
            Expr {
                ty: None,
                kind: ExprKind::ArrayLiteral(elements),
            }
        }
        Token::OpenBrace => {
            let block = parse_block(tokens);
            Expr {
                ty: None,
                kind: ExprKind::Compound(Box::new(block)),
            }
        }

        Token::StringLiteral(s) => {
            let len = s.chars().count() as i32;

            let elements = s
                .chars()
                .map(|c| Expr {
                    ty: Some(Type::Char),
                    kind: ExprKind::Constant(Const::Char(c)),
                })
                .collect::<Vec<_>>();

            Expr {
                ty: Some(Type::Array {
                    element_type: Box::new(Type::Char),
                    size: len,
                }),
                kind: ExprKind::ArrayLiteral(elements),
            }
        }

        Token::IntLiteral32(v) => Expr {
            ty: Some(Type::I32),
            kind: ExprKind::Constant(Const::I32(v)),
        },
        Token::IntLiteral64(v) => Expr {
            ty: Some(Type::I64),
            kind: ExprKind::Constant(Const::I64(v)),
        },
        Token::FloatLiteral64(v) => Expr {
            ty: Some(Type::F64),
            kind: ExprKind::Constant(Const::F64(v)),
        },

        Token::Identifier(name) => {
            if tokens.peek().unwrap() == Token::OpenParen {
                tokens.consume();
                let args = parse_argument_list(tokens);
                expect(Token::CloseParen, tokens);

                if name == "slice" {
                    if args.len() != 1 {
                        panic!("slice() expects exactly one argument");
                    }

                    Expr {
                        ty: None,
                        kind: ExprKind::SliceFromArray(Box::new(args.into_iter().next().unwrap())),
                    }
                } else if name == "len" {
                    if args.len() != 1 {
                        panic!("len() expects exactly one argument");
                    }

                    Expr {
                        ty: None,
                        kind: ExprKind::SliceLen(Box::new(args.into_iter().next().unwrap())),
                    }
                } else {
                    Expr {
                        ty: None,
                        kind: ExprKind::FunctionCall(name, args),
                    }
                }
            } else {
                Expr {
                    ty: None,
                    kind: ExprKind::Var(name),
                }
            }
        }

        tok @ (Token::Minus | Token::Tilde | Token::Not) => {
            let op = parse_unop(&tok);
            let inner = parse_factor(tokens);
            Expr {
                ty: inner.ty.clone(),
                kind: ExprKind::Unary(op, Box::new(inner)),
            }
        }

        Token::OpenParen => {
            let e = parse_expr(tokens, 0);
            expect(Token::CloseParen, tokens);
            e
        }

        t => panic!("Invalid factor {:?}", t),
    }
}

fn parse_argument_list(tokens: &mut Queue<Token>) -> Vec<Expr> {
    let mut args = Vec::new();
    if tokens.peek().unwrap() == Token::CloseParen {
        return args;
    }
    loop {
        args.push(parse_expr(tokens, 0));
        if tokens.peek().unwrap() == Token::Comma {
            tokens.consume();
        } else {
            break;
        }
    }
    args
}

fn parse_expr(tokens: &mut Queue<Token>, min_prec: i32) -> Expr {
    let mut left = parse_factor(tokens);

    loop {
        match tokens.peek().unwrap() {
            Token::OpenBracket => {
                tokens.consume();
                let index = parse_expr(tokens, 0);
                expect(Token::CloseBracket, tokens);
                left = Expr {
                    ty: None,
                    kind: ExprKind::ArrayIndex(Box::new(left), Box::new(index)),
                };
            }
            _ => break,
        }
    }

    while let Ok(Token::Keyword(ref kw)) = tokens.peek() {
        if kw == "as" {
            tokens.consume();
            let target = parse_type(tokens);
            left = Expr {
                ty: Some(target.clone()),
                kind: ExprKind::Cast {
                    expr: Box::new(left),
                    target,
                },
            };
        } else {
            break;
        }
    }

    loop {
        let next_token = match tokens.peek() {
            Ok(tok) => tok.clone(),
            Err(_) => break,
        };
        if !has_precedence(&next_token) {
            break;
        }
        let prec = precedence(&next_token);
        if prec < min_prec {
            break;
        }

        match next_token {
            Token::Assign => {
                tokens.consume();
                let right = parse_expr(tokens, prec);
                left = Expr {
                    ty: left.ty.clone(),
                    kind: ExprKind::Assign(Box::new(left), Box::new(right)),
                };
            }
            Token::QuestionMark => {
                let middle = parse_conditional_middle(tokens);
                let right = parse_expr(tokens, precedence(&Token::QuestionMark));
                left = Expr {
                    ty: right.ty.clone(),
                    kind: ExprKind::IfThenElse(Box::new(left), Box::new(middle), Box::new(right)),
                };
            }
            _ => {
                let op = parse_binop(&tokens.remove().unwrap()).unwrap();
                let right = parse_expr(tokens, prec + 1);
                left = Expr {
                    ty: left.ty.clone(),
                    kind: ExprKind::Binary(op, Box::new(left), Box::new(right)),
                };
            }
        }
    }

    left
}

fn parse_conditional_middle(tokens: &mut Queue<Token>) -> Expr {
    expect(Token::QuestionMark, tokens);
    let middle = parse_expr(tokens, 0);
    expect(Token::Colon, tokens);
    middle
}

fn has_precedence(tok: &Token) -> bool {
    is_token_binop(tok) || *tok == Token::Assign || *tok == Token::QuestionMark
}
fn is_token_binop(tok: &Token) -> bool {
    parse_binop(tok).is_ok()
}

fn precedence(tok: &Token) -> i32 {
    use Token::*;
    match tok {
        Star | Divide | Remainder => 50,
        Plus | Minus => 45,
        LessThan | LessOrEqual | GreaterThan | GreaterOrEqual => 35,
        Equal | NotEqual => 30,
        And => 10,
        Or => 5,
        QuestionMark => 3,
        Assign => 1,
        _ => panic!("{:?} has no precedence", tok),
    }
}

fn parse_unop(tok: &Token) -> UnaryOp {
    match tok {
        Token::Minus => UnaryOp::Negate,
        Token::Tilde => UnaryOp::Complement,
        Token::Not => UnaryOp::Not,
        Token::Decrement => todo!(),
        _ => panic!("Expected unary operator, got {:?}", tok),
    }
}

fn parse_binop(tok: &Token) -> Result<BinaryOp, ()> {
    use BinaryOp::*;
    match tok {
        Token::Plus => Ok(Add),
        Token::Minus => Ok(Subtract),
        Token::Star => Ok(Multiply),
        Token::Remainder => Ok(Remainder),
        Token::Divide => Ok(Divide),
        Token::And => Ok(And),
        Token::Or => Ok(Or),
        Token::Equal => Ok(Equal),
        Token::NotEqual => Ok(NotEqual),
        Token::LessThan => Ok(LessThan),
        Token::GreaterThan => Ok(GreaterThan),
        Token::LessOrEqual => Ok(LessOrEqual),
        Token::GreaterOrEqual => Ok(GreaterOrEqual),
        _ => Err(()),
    }
}

fn expect(expected: Token, tokens: &mut Queue<Token>) -> Token {
    let actual = tokens.remove().unwrap();
    if actual != expected {
        panic!("Syntax Error: Expected {:?} but got {:?}", expected, actual)
    } else {
        actual
    }
}
