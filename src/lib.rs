use std::{collections::HashSet, hash::Hash, iter::Peekable, str::Chars, sync::Arc};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Char,
    Fn,
    Custom(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Add,
    Sub,
    Mul,
    Div,

    GreaterThan,
    LessThan,
    Equals,
    GreaterOrEqual,
    LessOrEqual,

    Punct(char),

    Increment,
    Decrement,

    Int(i32),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),

    Ref(Arc<Token>),
    MutRef(Arc<Token>),
    UnboundFnRef(String),
    UnboundRef(String),
    UnboundMutRef(String),

    Const(Type),
    Mut(Type),
    Ident(String),
    Fn(String),
    FnIdent(String),
    AnonymousFn,
    FnEnd,
    FnOut,
    Loop,
    LoopEnd,
    LoopOut,
    If,
    Else,
    StdIn,
    StdOut,
    StdErr,

    NewLine,
    Unknown(String),
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(val) => {
                "int".hash(state);
                val.hash(state);
            }
            Self::Float(val) => {
                "float".hash(state);
                val.to_bits().hash(state);
            }
            Self::String(val) => {
                "string".hash(state);
                val.hash(state);
            }
            Self::Bool(val) => {
                "bool".hash(state);
                val.hash(state);
            }
            Self::Char(val) => {
                "char".hash(state);
                val.hash(state);
            }
            Self::Ref(token) => {
                "ref".hash(state);
                token.hash(state);
            }
            Self::MutRef(token) => {
                "mut-ref".hash(state);
                token.hash(state);
            }
            Self::UnboundFnRef(string) => {
                "unbound-fn-ref".hash(state);
                string.hash(state);
            }
            Self::UnboundRef(string) => {
                "unbound-ref".hash(state);
                string.hash(state);
            }
            Self::UnboundMutRef(string) => {
                "unbound-mut-ref".hash(state);
                string.hash(state);
            }
            Self::Const(ty) => {
                "const".hash(state);
                ty.hash(state);
            }
            Self::Mut(ty) => {
                "mut".hash(state);
                ty.hash(state);
            }
            Self::Ident(name) => {
                "ident".hash(state);
                name.hash(state);
            }
            Self::Fn(name) => {
                "fn".hash(state);
                name.hash(state);
            }
            Self::Unknown(string) => {
                "unknown".hash(state);
                string.hash(state);
            }
            _ => format!("{self:?}").to_lowercase().hash(state),
        }
    }
}

impl Eq for Token {}

pub struct Lexer<'a> {
    content: Peekable<Chars<'a>>,
    idents: HashSet<Arc<Token>>,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            content: content.chars().peekable(),
            idents: HashSet::new(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let mut word = String::new();
        let mut closure: Option<char> = None;

        while !match (
            word.as_str(),
            closure,
            word.chars().last(),
            self.content.peek().cloned(),
        ) {
            (_, None, _, Some(start @ '`')) => {
                closure = Some(start);
                false
            }
            (_, Some(start), _, Some(end)) => {
                if start == end {
                    self.content.next();
                    true
                } else {
                    false
                }
            }
            ("/#", _, _, _) => true,
            (_, _, Some(',' | ';'), _) => true,
            (_, _, Some(_), None) => true,
            (_, _, Some(_), Some(' ')) => true,
            (_, _, Some('#'), Some('a'..='z' | 'A'..='Z')) => false,
            (_, _, Some('#' | '@'), _) => true,
            (_, _, Some('\n'), _) => true,
            (_, _, Some(a @ ('+' | '-' | '*' | '/')), Some(b @ ('+' | '-' | '*' | '/'))) => {
                !(a == b)
            }
            (
                _,
                _,
                Some('+' | '-' | '*' | '/' | '>' | '<' | '='),
                Some('a'..='z' | 'A'..='Z' | '0'..='9'),
            ) => true,
            (
                _,
                _,
                Some('a'..='z' | 'A'..='Z' | '0'..='9'),
                Some('+' | '-' | '*' | '/' | '>' | '<' | '=' | '\n' | ',' | ';'),
            ) => true,
            (_, _, Some('a'..='z' | 'A'..='Z'), Some(':')) => true,
            _ => false,
        } {
            if let Some(ch) = self.content.next() {
                if closure.is_some() || ch != ' ' {
                    word.push(ch);
                }
            } else {
                return None;
            }
        }

        Some(match word.as_str() {
            "+" => Token::Add,
            "-" => Token::Sub,
            "*" => Token::Mul,
            "/" => Token::Div,

            ">" => Token::GreaterThan,
            "<" => Token::LessThan,
            "=" => Token::Equals,
            ">=" => Token::GreaterOrEqual,
            "<=" => Token::LessOrEqual,

            "?" => Token::If,
            ":" => Token::Else,
            "#" => Token::AnonymousFn,

            mark @ ("," | ";") => Token::Punct(
                mark.chars()
                    .next()
                    .expect("there should only be single char marks"),
            ),

            "++" => Token::Increment,
            "--" => Token::Decrement,

            "@" => Token::Loop,
            "/@" => Token::LoopEnd,
            "<<@" => Token::LoopOut,

            "/#" => Token::FnEnd,
            "<<#" => Token::FnOut,

            "<<" => Token::StdOut,
            ">>" => Token::StdIn,
            "<!" => Token::StdErr,

            "\n" => Token::NewLine,

            _ => {
                if let Ok(int) = word.parse::<i32>() {
                    Token::Int(int)
                } else if let Ok(float) = word.parse::<f64>() {
                    Token::Float(float)
                } else {
                    let mut iter = word.chars();

                    match (iter.next(), word.get(1..)) {
                        (Some('`'), Some(string)) => Token::String(string.to_string()),
                        (Some('$'), Some(ident)) => {
                            let token = Token::Ident(ident.to_string());
                            self.idents.insert(Arc::new(token.clone()));
                            token
                        }
                        (Some('#'), Some(ident)) => {
                            let token = Token::Fn(ident.to_string());
                            self.idents.insert(Arc::new(token.clone()));
                            token
                        }
                        (Some(':'), Some(ident)) => {
                            let (ident, is_mutable) = match iter.next() {
                                Some('~') => (ident.get(1..), true),
                                _ => (Some(ident), false),
                            };

                            let tipo = match ident {
                                Some("i" | "int") => Type::Int,
                                Some("f" | "float") => Type::Float,
                                Some("s" | "string") => Type::String,
                                Some("b" | "bool") => Type::Bool,
                                Some("c" | "char") => Type::Char,
                                Some("fn" | "function") => Type::Fn,
                                Some(ident) => Type::Custom(ident.to_string()),
                                None => unreachable!(),
                            };

                            if is_mutable {
                                Token::Mut(tipo)
                            } else {
                                Token::Const(tipo)
                            }
                        }
                        (Some('~'), Some(ident)) => {
                            if let Some(i) = self.idents.iter().find(|i| match i.as_ref() {
                                Token::Ident(i) => i.as_str() == ident,
                                _ => false,
                            }) {
                                Token::MutRef(i.clone())
                            } else {
                                Token::UnboundMutRef(ident.to_string())
                            }
                        }
                        _ => {
                            if word.ends_with('#') {
                                if let Some(i) = self.idents.iter().find(|i| match i.as_ref() {
                                    Token::Fn(i) => match word.get(..(word.len() - 1)) {
                                        Some(name) => name == i.as_str(),
                                        None => false,
                                    },
                                    _ => false,
                                }) {
                                    Token::Ref(i.clone())
                                } else {
                                    Token::UnboundFnRef(word)
                                }
                            } else if let Some(i) = self.idents.iter().find(|i| match i.as_ref() {
                                Token::Ident(i) => i.as_str() == word.as_str(),
                                _ => false,
                            }) {
                                Token::Ref(i.clone())
                            } else {
                                Token::UnboundRef(word)
                            }
                        }
                    }
                }
            }
        })
    }
}
