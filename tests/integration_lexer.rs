use weird_lexer::*;

macro_rules! assert_tk {
    ($expr: expr, = $token: ident($name: literal)) => {
        match $expr {
            Some(Token::$token(name, id)) => {
                assert_eq!(name, $name);
                Some(Token::$token(name, id))
            }
            found => panic!(
                "assertion failed: `((left) = right)`\n  left: `{found:?}`,\n right: `{:?}`",
                Some(Token::$token($name.into(), 0)),
            ),
        }
    };
    ($expr: expr, is $ref: ident($token: ident)) => {
        match $expr {
            Some(Token::$ref(token)) => {
                assert_eq!(token.as_ref(), $token.as_ref().unwrap())
            }
            token => panic!(
                "assertion failed: `((left) is right)`\n  left: `{token:?}`,\n right: `{:?}`",
                Token::$ref(std::sync::Arc::new($token.unwrap()))
            ),
        }
    };
}

#[test]
fn add_numbers() {
    for program in ["1+2", "1+2 ", "1 + 2", " 1+2"] {
        let mut lexer = Lexer::new(program);
        assert_eq!(lexer.next(), Some(Token::Int(1)));
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_eq!(lexer.next(), Some(Token::Int(2)));
        assert_eq!(lexer.next(), None);
    }
}

#[test]
fn operators() {
    let mut lexer = Lexer::new("1 + 10 - 12 * 8 / 2");
    assert_eq!(lexer.next(), Some(Token::Int(1)));
    assert_eq!(lexer.next(), Some(Token::Add));
    assert_eq!(lexer.next(), Some(Token::Int(10)));
    assert_eq!(lexer.next(), Some(Token::Sub));
    assert_eq!(lexer.next(), Some(Token::Int(12)));
    assert_eq!(lexer.next(), Some(Token::Mul));
    assert_eq!(lexer.next(), Some(Token::Int(8)));
    assert_eq!(lexer.next(), Some(Token::Div));
    assert_eq!(lexer.next(), Some(Token::Int(2)));
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
        assert_tk!(lexer.next(), = Ident("var"));
        assert_eq!(lexer.next(), Some(Token::Int(10)));
        assert_eq!(lexer.next(), None);
    }
}

#[test]
fn declare_variables_with_types() {
    for program in [
        "$some :int 20 ",
        " $some:int 20",
        " $some:i 20",
        "$some :i 20",
    ] {
        let mut lexer = Lexer::new(program);
        assert_tk!(lexer.next(), = Ident("some"));
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
    assert_tk!(lexer.next(), = Ident("statement"));
    assert_eq!(lexer.next(), Some(Token::String("hello world".to_string())));
    assert_eq!(lexer.next(), None);
}

#[test]
fn mutable_variables() {
    let mut lexer = Lexer::new("$var:~i 10");
    assert_tk!(lexer.next(), = Ident("var"));
    assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::Int(10)));
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("$var :~int 20");
    assert_tk!(lexer.next(), = Ident("var"));
    assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::Int(20)));
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("$var :~string `cole`");
    assert_tk!(lexer.next(), = Ident("var"));
    assert_eq!(lexer.next(), Some(Token::Mut(Type::String)));
    assert_eq!(lexer.next(), Some(Token::String("cole".to_string())));
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("$const :string `Cole Davis is a very cool guy.`");
    assert_tk!(lexer.next(), = Ident("const"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::String)));
    assert_eq!(
        lexer.next(),
        Some(Token::String("Cole Davis is a very cool guy.".to_string()))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn function_declaration() {
    let mut lexer = Lexer::new(" #main /# ");
    assert_tk!(lexer.next(), = Fn("main"));
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
    let hello_world = assert_tk!(lexer.next(), = Fn("hello_world"));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::StdOut));
    assert_eq!(lexer.next(), Some(Token::String("Hello World".to_string())));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), &hello_world.unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }

    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None)
}

#[test]
fn print_fn() {
    let program = "
        #print $a
          <<# a
        /#

        print# 10
        print# `Hello, World!`
";
    let mut lexer = Lexer::new(program);

    assert_eq!(lexer.next(), Some(Token::NewLine));
    let print = assert_tk!(lexer.next(), = Fn("print"));
    let a = assert_tk!(lexer.next(), = Ident("a"));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), &a.unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), print.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Int(10)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), print.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(
        lexer.next(),
        Some(Token::String("Hello, World!".to_string()))
    );
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None);
}

#[test]
fn print_one_line_fn() {
    let program = "
#print $n:~int $a:string @ ~n-- > 0 <<# a /@ /#
print# 10, `Hello, World!`
";
    let mut lexer = Lexer::new(program);
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let print = assert_tk!(lexer.next(), = Fn("print"));
    let n = assert_tk!(lexer.next(), = Ident("n"));
    assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
    let a = assert_tk!(lexer.next(), = Ident("a"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::String)));
    assert_eq!(lexer.next(), Some(Token::Loop));
    assert_tk!(lexer.next(), is MutRef(n));
    assert_eq!(lexer.next(), Some(Token::Decrement));
    assert_eq!(lexer.next(), Some(Token::GreaterThan));
    assert_eq!(lexer.next(), Some(Token::Int(0)));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_tk!(lexer.next(), is Ref(a));
    assert_eq!(lexer.next(), Some(Token::LoopEnd));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_tk!(lexer.next(), is Ref(print));
    assert_eq!(lexer.next(), Some(Token::Int(10)));
    assert_eq!(lexer.next(), Some(Token::Punct(',')));
    assert_eq!(
        lexer.next(),
        Some(Token::String("Hello, World!".to_string()))
    );
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None);
}

#[test]
fn print_loop() {
    let program = "
#print $n:int $a:string
  $n:~int n
  @ ~n-- > 0
    <<# a
  /@
/#

print# 10, `Hello, World!`
";
    let mut lexer = Lexer::new(program);
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let print = assert_tk!(lexer.next(), = Fn("print"));
    let n = assert_tk!(lexer.next(), = Ident("n"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    let a = assert_tk!(lexer.next(), = Ident("a"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::String)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let n_inner = assert_tk!(lexer.next(), = Ident("n"));
    assert_ne!(n, n_inner);
    assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
    assert_tk!(lexer.next(), is Ref(n));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::Loop));
    assert_tk!(lexer.next(), is MutRef(n_inner));
    assert_eq!(lexer.next(), Some(Token::Decrement));
    assert_eq!(lexer.next(), Some(Token::GreaterThan));
    assert_eq!(lexer.next(), Some(Token::Int(0)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_tk!(lexer.next(), is Ref(a));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::LoopEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_tk!(lexer.next(), is Ref(print));
    assert_eq!(lexer.next(), Some(Token::Int(10)));
    assert_eq!(lexer.next(), Some(Token::Punct(',')));
    assert_eq!(
        lexer.next(),
        Some(Token::String("Hello, World!".to_string()))
    );
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None);
}

#[test]
fn fibbonacci() {
    let program = "
#fibonacci $n:int
  ? n < 2
    <<# n
  : <<# fibonacci# n - 1, + fibonacci# n - 2
/#
";
    let mut lexer = Lexer::new(program);
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let fibonacci = assert_tk!(lexer.next(), = Fn("fibonacci"));
    let n = assert_tk!(lexer.next(), = Ident("n"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::If));
    assert_tk!(lexer.next(), is Ref(n));
    assert_eq!(lexer.next(), Some(Token::LessThan));
    assert_eq!(lexer.next(), Some(Token::Int(2)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_tk!(lexer.next(), is Ref(n));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::Else));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_tk!(lexer.next(), is Ref(fibonacci));
    assert_tk!(lexer.next(), is Ref(n));
    assert_eq!(lexer.next(), Some(Token::Sub));
    assert_eq!(lexer.next(), Some(Token::Int(1)));
    assert_eq!(lexer.next(), Some(Token::Punct(',')));
    assert_eq!(lexer.next(), Some(Token::Add));
    assert_tk!(lexer.next(), is Ref(fibonacci));
    assert_tk!(lexer.next(), is Ref(n));
    assert_eq!(lexer.next(), Some(Token::Sub));
    assert_eq!(lexer.next(), Some(Token::Int(2)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None);
}

#[test]
fn lambda_fn() {
    for program in [
        "# $a:int $b:int <<# a + b /#",
        "# $a:int $b:int <<# a + b /#",
    ] {
        let mut lexer = Lexer::new(program);
        assert_eq!(lexer.next(), Some(Token::AnonymousFn));
        let a = assert_tk!(lexer.next(), = Ident("a"));
        assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
        let b = assert_tk!(lexer.next(), = Ident("b"));
        assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
        assert_eq!(lexer.next(), Some(Token::FnOut));
        assert_tk!(lexer.next(), is Ref(a));
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_tk!(lexer.next(), is Ref(b));
        assert_eq!(lexer.next(), Some(Token::FnEnd));
        assert_eq!(lexer.next(), None);
    }
}

#[test]
fn lambda_fn_call() {
    let program = "
#higher_order $fn:fn
  <<# fn# 10
/#

higher_order# # $a:int <<# a * 20 /#
    ";
    let mut lexer = Lexer::new(program);
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let higher_order = assert_tk!(lexer.next(), = Fn("higher_order"));
    let fn_ = assert_tk!(lexer.next(), = FnIdent("fn"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Fn)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_tk!(lexer.next(), is Ref(fn_));
    assert_eq!(lexer.next(), Some(Token::Int(10)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_tk!(lexer.next(), is Ref(higher_order));
    assert_eq!(lexer.next(), Some(Token::AnonymousFn));
    let a = assert_tk!(lexer.next(), = Ident("a"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_tk!(lexer.next(), is Ref(a));
    assert_eq!(lexer.next(), Some(Token::Mul));
    assert_eq!(lexer.next(), Some(Token::Int(20)));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None);
}

#[test]
fn else_if() {
    for program in [
        "
#main $a:int
  ? a = 1
    <<# 1
  :? a = 2
    <<# 2
  : <<# 3
  /?
/#",
        "
#main$a:int
?a=1
<<#1
:?a=2
<<#2
:<<#3
/?
/#",
    ] {
        let mut lexer = Lexer::new(program);
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_tk!(lexer.next(), = Fn("main"));
        let a = assert_tk!(lexer.next(), = Ident("a"));
        assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::If));
        assert_tk!(lexer.next(), is Ref(a));
        assert_eq!(lexer.next(), Some(Token::Equals));
        assert_eq!(lexer.next(), Some(Token::Int(1)));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::FnOut));
        assert_eq!(lexer.next(), Some(Token::Int(1)));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::ElseIf));
        assert_tk!(lexer.next(), is Ref(a));
        assert_eq!(lexer.next(), Some(Token::Equals));
        assert_eq!(lexer.next(), Some(Token::Int(2)));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::FnOut));
        assert_eq!(lexer.next(), Some(Token::Int(2)));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::Else));
        assert_eq!(lexer.next(), Some(Token::FnOut));
        assert_eq!(lexer.next(), Some(Token::Int(3)));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::IfEnd));
        assert_eq!(lexer.next(), Some(Token::NewLine));
        assert_eq!(lexer.next(), Some(Token::FnEnd));
        assert_eq!(lexer.next(), None);
    }

    let program = "#main$a:int?a=1<<#1:?a=2<<#2:<<#3/?/#";
    let mut lexer = Lexer::new(program);
    assert_tk!(lexer.next(), = Fn("main"));
    let a = assert_tk!(lexer.next(), = Ident("a"));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::If));
    assert_tk!(lexer.next(), is Ref(a));
    assert_eq!(lexer.next(), Some(Token::Equals));
    assert_eq!(lexer.next(), Some(Token::Int(1)));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_eq!(lexer.next(), Some(Token::Int(1)));
    assert_eq!(lexer.next(), Some(Token::ElseIf));
    assert_tk!(lexer.next(), is Ref(a));
    assert_eq!(lexer.next(), Some(Token::Equals));
    assert_eq!(lexer.next(), Some(Token::Int(2)));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_eq!(lexer.next(), Some(Token::Int(2)));
    assert_eq!(lexer.next(), Some(Token::Else));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    assert_eq!(lexer.next(), Some(Token::Int(3)));
    assert_eq!(lexer.next(), Some(Token::IfEnd));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), None);
}
