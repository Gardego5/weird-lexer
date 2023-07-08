use wierd_lexer::*;

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
    let print = lexer.next();
    assert_eq!(print, Some(Token::Fn("print".to_string())));
    let a = lexer.next();
    assert_eq!(a, Some(Token::Ident("a".to_string())));
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
print# 10 `Hello, World!`
";
    let mut lexer = Lexer::new(program);
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let print = lexer.next();
    assert_eq!(print, Some(Token::Fn("print".to_string())));
    let n = lexer.next();
    assert_eq!(n, Some(Token::Ident("n".to_string())));
    assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
    let a = lexer.next();
    assert_eq!(a, Some(Token::Ident("a".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::String)));
    assert_eq!(lexer.next(), Some(Token::Loop));
    match lexer.next() {
        Some(Token::MutRef(token)) => assert_eq!(token.as_ref(), &n.unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Decrement));
    assert_eq!(lexer.next(), Some(Token::GreaterThan));
    assert_eq!(lexer.next(), Some(Token::Int(0)));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), &a.unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::LoopEnd));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), print.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Int(10)));
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

print# 10 `Hello, World!`
";
    let mut lexer = Lexer::new(program);
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let print = lexer.next();
    assert_eq!(print, Some(Token::Fn("print".to_string())));
    let n = lexer.next();
    assert_eq!(n, Some(Token::Ident("n".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    let a = lexer.next();
    assert_eq!(a, Some(Token::Ident("a".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::String)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    let n_inner = lexer.next();
    assert_ne!(n, n_inner, "n_inner should be distinct from n");
    assert_eq!(n_inner, Some(Token::Ident("n".to_string())));
    assert_eq!(lexer.next(), Some(Token::Mut(Type::Int)));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), n.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::Loop));
    match lexer.next() {
        Some(Token::MutRef(token)) => assert_eq!(token.as_ref(), n_inner.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Decrement));
    assert_eq!(lexer.next(), Some(Token::GreaterThan));
    assert_eq!(lexer.next(), Some(Token::Int(0)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), a.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::LoopEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), print.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Int(10)));
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
    let fibonacci = lexer.next();
    assert_eq!(fibonacci, Some(Token::Fn("fibonacci".to_string())));
    let n = lexer.next();
    assert_eq!(n, Some(Token::Ident("n".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::If));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), n.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::LessThan));
    assert_eq!(lexer.next(), Some(Token::Int(2)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), n.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::Else));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), fibonacci.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), n.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Sub));
    assert_eq!(lexer.next(), Some(Token::Int(1)));
    assert_eq!(lexer.next(), Some(Token::Punct(',')));
    assert_eq!(lexer.next(), Some(Token::Add));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), fibonacci.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), n.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Sub));
    assert_eq!(lexer.next(), Some(Token::Int(2)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None);
}

#[test]
fn lambda_fn() {
    let program = "# $a:int $b:int <<# a + b /#";
    let mut lexer = Lexer::new(program);
    assert_eq!(lexer.next(), Some(Token::AnonymousFn));
    let a = lexer.next();
    assert_eq!(a, Some(Token::Ident("a".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    let b = lexer.next();
    assert_eq!(b, Some(Token::Ident("b".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), a.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Add));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), b.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), None);
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
    let higher_order = lexer.next();
    assert_eq!(higher_order, Some(Token::Fn("higher_order".to_string())));
    let fn_ = lexer.next();
    assert_eq!(fn_, Some(Token::FnIdent("fn".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Fn)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), fn_.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Int(10)));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), higher_order.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::AnonymousFn));
    let a = lexer.next();
    assert_eq!(a, Some(Token::Ident("a".to_string())));
    assert_eq!(lexer.next(), Some(Token::Const(Type::Int)));
    assert_eq!(lexer.next(), Some(Token::FnOut));
    match lexer.next() {
        Some(Token::Ref(token)) => assert_eq!(token.as_ref(), a.as_ref().unwrap()),
        token => panic!("Expected Token::Ref, got {:?}", token),
    }
    assert_eq!(lexer.next(), Some(Token::Mul));
    assert_eq!(lexer.next(), Some(Token::Int(20)));
    assert_eq!(lexer.next(), Some(Token::FnEnd));
    assert_eq!(lexer.next(), Some(Token::NewLine));
    assert_eq!(lexer.next(), None);
}
