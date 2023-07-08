use std::{collections::HashSet, hash::Hash, iter::Peekable, str::Chars, sync::Arc};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Char,
    Function,
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
                                Some("fn" | "function") => Type::Function,
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

fn main() {
    let program = "
#print $a
  <<# a
/#

print# 10
print# `Hello, World!`
    ";
    println!("{program}");
    let tokens: Vec<Token> = Lexer::new(program).collect();
    println!("{tokens:?}");

    let program = "
#print $n:int $a:string
  $n:~int n
  @ ~n-- > 0
    <<# a
  /@
/#

print# 10 `Hello, World!`
    ";
    println!("{program}");
    let tokens: Vec<Token> = Lexer::new(program).collect();
    println!("{tokens:?}");

    let program = "
#print $n:~int $a:string @ ~n-- > 0 <<# a /@ /#
print# 10 `Hello, World!`
    ";
    println!("{program}");
    let tokens: Vec<Token> = Lexer::new(program).collect();
    println!("{tokens:?}");

    let program = "
#fibonacci $n:int
  ? n < 2
    <<# n
  : <<# fibonacci# n - 1, + fibonacci# n - 2
/#
    ";
    println!("{program}");
    let tokens: Vec<Token> = Lexer::new(program).collect();
    println!("{tokens:?}");

    let program = "# $a:int $b:int <<# a + b /#";
    println!("{program}");
    let tokens: Vec<Token> = Lexer::new(program).collect();
    println!("{tokens:?}");

    let program = "
#higher_order $fn:fn
  <<# fn# 10
/#

#higher_order # $a:int <<# a * 20 /#
    ";
    println!("{program}");
    let tokens: Vec<Token> = Lexer::new(program).collect();
    println!("{tokens:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_numbers() {
        for test in ["1+2", "1+2 ", "1 + 2", " 1+2"] {
            let mut lexer = Lexer::new(test);
            assert_eq!(lexer.next(), Some(Token::Int(1)));
            assert_eq!(lexer.next(), Some(Token::Add));
            assert_eq!(lexer.next(), Some(Token::Int(2)));
            assert_eq!(lexer.next(), None);
        }
    }

    #[test]
    fn operators() {
        let mut lexer = Lexer::new("+-*/");
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_eq!(lexer.next(), Some(Token::Sub));
        assert_eq!(lexer.next(), Some(Token::Mul));
        assert_eq!(lexer.next(), Some(Token::Div));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("20 / 12");
        assert_eq!(lexer.next(), Some(Token::Int(20)));
        assert_eq!(lexer.next(), Some(Token::Div));
        assert_eq!(lexer.next(), Some(Token::Int(12)));
    }

    #[test]
    fn declare_variables() {
        for test in ["$var 10", "$var 10 ", " $var 10  ", " $var   10 "] {
            let mut lexer = Lexer::new(test);
            assert_eq!(lexer.next(), Some(Token::Ident("var".to_string())));
            assert_eq!(lexer.next(), Some(Token::Int(10)));
            assert_eq!(lexer.next(), None);
        }
    }

    #[test]
    fn declare_variables_with_types() {
        for test in [
            "$some :int 20 ",
            " $some:int 20",
            " $some:i 20",
            "$some :i 20",
        ] {
            let mut lexer = Lexer::new(test);
            assert_eq!(lexer.next(), Some(Token::Ident("some".to_string())));
            assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
            assert_eq!(lexer.next(), Some(Token::Int(20)));
            assert_eq!(lexer.next(), None);
        }
    }

    #[test]
    fn comparison_operators() {
        let mut lexer = Lexer::new("1 > 2");
        assert_eq!(lexer.next(), Some(Token::Int(1)));
        assert_eq!(lexer.next(), Some(Token::GreaterThan));
        assert_eq!(lexer.next(), Some(Token::Int(2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("1 >= 2 ");
        assert_eq!(lexer.next(), Some(Token::Int(1)));
        assert_eq!(lexer.next(), Some(Token::GreaterOrEqual));
        assert_eq!(lexer.next(), Some(Token::Int(2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new(" 1>=2");
        assert_eq!(lexer.next(), Some(Token::Int(1)));
        assert_eq!(lexer.next(), Some(Token::GreaterOrEqual));
        assert_eq!(lexer.next(), Some(Token::Int(2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("1 = 2");
        assert_eq!(lexer.next(), Some(Token::Int(1)));
        assert_eq!(lexer.next(), Some(Token::Equals));
        assert_eq!(lexer.next(), Some(Token::Int(2)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn increment_decrement() {
        let mut lexer = Lexer::new("++ --");
        assert_eq!(lexer.next(), Some(Token::Increment));
        assert_eq!(lexer.next(), Some(Token::Decrement));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("20--");
        assert_eq!(lexer.next(), Some(Token::Int(20)));
        assert_eq!(lexer.next(), Some(Token::Decrement));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("++20.0");
        assert_eq!(lexer.next(), Some(Token::Increment));
        assert_eq!(lexer.next(), Some(Token::Float(20.)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn new_line() {
        let mut lexer = Lexer::new("\n");
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn string() {
        let mut lexer = Lexer::new("`hello world`");
        assert_eq!(lexer.next(), Some(Token::String("hello world".to_string())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("$statement `hello world` ");
        assert_eq!(lexer.next(), Some(Token::Ident("statement".to_string())));
        assert_eq!(lexer.next(), Some(Token::String("hello world".to_string())));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn mutable_variables() {
        let mut lexer = Lexer::new("$var:~i 10");
        assert_eq!(lexer.next(), Some(Token::Ident("var".to_string())));
        assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
        assert_eq!(lexer.next(), Some(Token::Int(10)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("$var :~int 20");
        assert_eq!(lexer.next(), Some(Token::Ident("var".to_string())));
        assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
        assert_eq!(lexer.next(), Some(Token::Int(20)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("$var :~string `cole`");
        assert_eq!(lexer.next(), Some(Token::Ident("var".to_string())));
        assert_eq!(lexer.next(), Some(Token::Mut(Type::String)));
        assert_eq!(lexer.next(), Some(Token::String("cole".to_string())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("$should_be_const :~string `Cole Davis is a very cool guy.`");
        assert_eq!(
            lexer.next(),
            Some(Token::Ident("should_be_const".to_string()))
        );
        assert_eq!(lexer.next(), Some(Token::Mut(Type::String)));
        assert_eq!(
            lexer.next(),
            Some(Token::String("Cole Davis is a very cool guy.".to_string()))
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn function_declaration() {
        let mut lexer = Lexer::new(" #main /# ");
        assert_eq!(lexer.next(), Some(Token::Fn("main".to_string())));
        assert_eq!(lexer.next(), Some(Token::FnEnd));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn function_calls() {
        let mut lexer = Lexer::new(
            "
            #hello_world
                << `Hello World`
            /#

            hello_world#
            ",
        );
        assert_eq!(lexer.next(), Some(Token::NewLine));
        let declaration = lexer.next();
        assert_eq!(declaration, Some(Token::Fn("hello_world".to_string())));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::StdOut));
        assert_eq!(lexer.next(), Some(Token::String("Hello World".to_string())));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::FnEnd));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        match lexer.next() {
            Some(Token::Ref(token)) => assert_eq!(token.as_ref(), &declaration.unwrap()),
            token => panic!("Expected Token::Ref, got {:?}", token),
        }

        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), None)
    }
}
