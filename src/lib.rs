use std::{
    collections::{hash_map::DefaultHasher, HashSet},
    hash::{Hash, Hasher},
    iter::Peekable,
    rc::Rc,
    str::Chars,
    sync::Mutex,
};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Char,
    Fn,
    Custom(String, u64),
}

#[derive(Clone, Debug)]
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

    Ref(Rc<Token>),
    MutRef(Rc<Token>),
    UnboundFnRef(String),
    UnboundRef(String),
    UnboundMutRef(String),

    Const(Type),
    Mut(Type),
    Ident(String, u64),
    DuplicateSymbol(String, u64, Rc<Token>),
    Fn(String, u64, Rc<Mutex<Scope>>),
    AnonymousFn(u64, Rc<Mutex<Scope>>),
    FnEnd,
    FnOut,
    Loop(Rc<Mutex<Scope>>),
    LoopEnd,
    LoopOut,
    If(Rc<Mutex<Scope>>),
    Else(Rc<Mutex<Scope>>),
    ElseIf(Rc<Mutex<Scope>>),
    IfEnd,
    StdIn,
    StdOut,
    StdErr,

    NewLine,
    Unknown(String),
}

impl Token {
    fn unique() -> u64 {
        std::hash::BuildHasher::build_hasher(&std::collections::hash_map::RandomState::new())
            .finish()
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Ident(name, _id) => Some(name),
            Self::Fn(name, _id, _scope) => Some(name),
            _ => None,
        }
    }

    pub fn id(&self) -> Option<u64> {
        match self {
            Self::Ident(_name, id) => Some(id.clone()),
            Self::Fn(_name, id, _scope) => Some(id.clone()),
            Self::AnonymousFn(id, _scope) => Some(id.clone()),
            _ => None,
        }
    }

    pub fn ref_to(&self) -> Option<Rc<Token>> {
        match self {
            Self::Ref(to) => Some(to.clone()),
            Self::MutRef(to) => Some(to.clone()),
            _ => None,
        }
    }
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
            Self::Ident(name, id) => {
                id.hash(state);
                "ident".hash(state);
                name.hash(state);
            }
            Self::Fn(name, id, _scope) => {
                id.hash(state);
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

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        let hash = |value: &Self| {
            let mut hasher = DefaultHasher::new();
            value.hash(&mut hasher);
            hasher.finish()
        };
        hash(self) == hash(other)
    }
}

impl Eq for Token {}

#[derive(PartialEq, Eq, Clone, Debug, Default)]
pub struct Scope {
    symbols: HashSet<Rc<Token>>,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    content: Peekable<Chars<'a>>,
    scopes: Vec<Rc<Mutex<Scope>>>,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a str) -> Self {
        Self {
            content: content.chars().peekable(),
            scopes: vec![Rc::new(Mutex::new(Scope::default()))],
        }
    }

    pub fn full_scope(&self) -> HashSet<Rc<Token>> {
        self.scopes.iter().fold(HashSet::new(), |acc, scope| {
            let mut symbols = scope.lock().unwrap().symbols.clone();
            let symbol_names: HashSet<String> = symbols
                .iter()
                .filter_map(|tk| match tk.name() {
                    Some(name) => Some(name.to_string()),
                    None => None,
                })
                .collect();

            symbols.extend(acc.into_iter().filter(|tk| match tk.name() {
                Some(name) => !symbol_names.contains(name.clone()),
                None => true,
            }));

            symbols
        })
    }

    pub fn new_scope(&mut self) -> Rc<Mutex<Scope>> {
        let scope = Rc::new(Mutex::new(Scope::default()));
        self.scopes.push(scope.clone());
        scope
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
            ("/#" | "?" | ":?", _, _, _) => true,
            (_, _, Some(_), None | Some(' ')) => true,
            (_, _, Some('#'), Some('a'..='z' | 'A'..='Z')) => false,
            (_, _, Some(':'), Some('?')) => false,
            (_, _, Some('#' | '@' | '\n' | ',' | ';'), _) => true,
            (
                _,
                _,
                Some('+' | '-' | '*' | '/' | '>' | '<' | '='),
                Some('a'..='z' | 'A'..='Z' | '0'..='9'),
            ) => true,
            (
                _,
                _,
                Some('a'..='z' | 'A'..='Z' | '0'..='9' | ':' | '?'),
                Some('+' | '-' | '*' | '/' | '>' | '<' | '=' | '\n' | ',' | ';' | '$' | ':' | '?'),
            ) => true,
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

            "?" => Token::If(self.new_scope()),
            ":" => Token::Else(self.new_scope()),
            ":?" => Token::ElseIf(self.new_scope()),
            "/?" => Token::IfEnd,

            "#" => Token::AnonymousFn(Token::unique(), self.new_scope()),

            "++" => Token::Increment,
            "--" => Token::Decrement,

            "@" => Token::Loop(self.new_scope()),
            "/@" => Token::LoopEnd,
            "<<@" => Token::LoopOut,

            "/#" => Token::FnEnd,
            "<<#" => Token::FnOut,

            "<<" => Token::StdOut,
            ">>" => Token::StdIn,
            "<!" => Token::StdErr,

            "\n" => Token::NewLine,

            mark @ ("," | ";") => Token::Punct(mark.chars().next().unwrap()),

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
                            let token = Token::Ident(ident.to_string(), Token::unique());

                            if let Some(existing) = self
                                .scopes
                                .last_mut()
                                .unwrap()
                                .lock()
                                .unwrap()
                                .symbols
                                .iter()
                                .find(|tk| tk.name().unwrap() == ident)
                            {
                                return Some(Token::DuplicateSymbol(
                                    ident.to_string(),
                                    Token::unique(),
                                    existing.clone(),
                                ));
                            }

                            self.scopes
                                .last_mut()
                                .unwrap()
                                .lock()
                                .unwrap()
                                .symbols
                                .insert(Rc::new(token.clone()));

                            token
                        }
                        (Some('#'), Some(ident)) => {
                            let token = Rc::new(Token::Fn(
                                ident.to_string(),
                                Token::unique(),
                                self.new_scope(),
                            ));
                            self.scopes
                                .last_mut()
                                .unwrap()
                                .lock()
                                .unwrap()
                                .symbols
                                .insert(token.clone());
                            token.as_ref().to_owned()
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
                                Some(ident) => Type::Custom(ident.to_string(), Token::unique()),
                                None => unreachable!(),
                            };

                            if is_mutable {
                                Token::Mut(tipo)
                            } else {
                                Token::Const(tipo)
                            }
                        }
                        (Some('~'), Some(ident)) => {
                            if let Some(i) = self.full_scope().iter().find(|i| match i.as_ref() {
                                Token::Ident(i, _id) => i.as_str() == ident,
                                _ => false,
                            }) {
                                Token::MutRef(i.clone())
                            } else {
                                Token::UnboundMutRef(ident.to_string())
                            }
                        }
                        _ => {
                            if word.ends_with('#') {
                                let name = match word.get(..(word.len() - 1)) {
                                    Some(name) => name,
                                    None => return Some(Token::Unknown(word)),
                                };

                                if let Some(ident) =
                                    self.full_scope().iter().find(|ident| match ident.as_ref() {
                                        Token::Fn(ident, _id, _scope) => name == ident.as_str(),
                                        _ => false,
                                    })
                                {
                                    Token::Ref(ident.clone())
                                } else {
                                    Token::UnboundFnRef(name.to_string())
                                }
                            } else if let Some(i) =
                                self.full_scope().iter().find(|i| match i.as_ref() {
                                    Token::Ident(i, _id) => i.as_str() == word.as_str(),
                                    _ => false,
                                })
                            {
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
