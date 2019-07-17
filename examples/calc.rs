use cyclotron::eager::Memoized as _;
use cyclotron::lazy_set::LazySet as _;

use std::collections::BTreeSet;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Token {
    Op(char),
    Lit(i32),
}

impl Token {
    fn op(self) -> Option<char> {
        match self {
            Token::Op(op) => Some(op),
            _ => None,
        }
    }
    fn lit(self) -> Option<i32> {
        match self {
            Token::Lit(x) => Some(x),
            _ => None,
        }
    }
}

fn eat<T>(tokens: &[Token], f: impl FnOnce(Token) -> Option<T>) -> Option<(T, &[Token])> {
    tokens
        .split_first()
        .and_then(|(&token, tokens)| f(token).map(|x| (x, tokens)))
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Expr {
    Const(i32),
    Bin(Rc<Expr>, char, Rc<Expr>),
}

impl Expr {
    fn eval(&self) -> i32 {
        match self {
            Expr::Const(x) => *x,
            Expr::Bin(a, '+', b) => a.eval() + b.eval(),
            Expr::Bin(a, '-', b) => a.eval() - b.eval(),
            Expr::Bin(a, '*', b) => a.eval() * b.eval(),
            Expr::Bin(a, '/', b) => a.eval() / b.eval(),
            Expr::Bin(..) => unimplemented!(),
        }
    }
}

fn test<'a>(mut parse_expr: impl FnMut(&'a [Token]) -> BTreeSet<(Rc<Expr>, &'a [Token])>) {
    let mut parse_and_eval = |tokens| {
        let parses = parse_expr(tokens);
        eprintln!("parse_expr({:?}) =", tokens);
        for (e, after) in &parses {
            eprintln!("    | {:?} // remaining: {:?}", e, after);
        }
        // Only try to eval full parses.
        for (e, after) in parses {
            if after.is_empty() {
                eprintln!("eval({:?}) = {}", e, e.eval())
            }
        }
    };
    parse_and_eval(&[Token::Lit(123)]);
    eprintln!();
    parse_and_eval(&[Token::Lit(1), Token::Op('+'), Token::Lit(2)]);
    eprintln!();
    parse_and_eval(&[
        Token::Lit(1),
        Token::Op('+'),
        Token::Lit(2),
        Token::Op('*'),
        Token::Lit(3),
    ]);
}

fn eager<'a>() {
    use cyclotron::eager::memoize;

    let mut parse_expr = memoize(
        |parse_expr, tokens: &'a [Token]| -> BTreeSet<(Rc<Expr>, &'a [Token])> {
            let mut parses = BTreeSet::new();

            // `expr ::= expr OP expr`
            for (lhs, tokens) in parse_expr.call(tokens).clone() {
                if let Some((op, tokens)) = eat(tokens, Token::op) {
                    for (rhs, tokens) in parse_expr.call(tokens).clone() {
                        parses.insert((Rc::new(Expr::Bin(lhs.clone(), op, rhs)), tokens));
                    }
                }
            }

            // `expr ::= LIT`
            if let Some((x, tokens)) = eat(tokens, Token::lit) {
                parses.insert((Rc::new(Expr::Const(x)), tokens));
            }

            parses
        },
    );
    test(|tokens| parse_expr.call(tokens).clone());
}

fn lazy_set<'a>() {
    use cyclotron::lazy_set::{call, memoize_eagerly};

    let parse_expr = call;
    let parse_expr = |tokens: &'a [Token]| {
        // `expr ::= expr OP expr`
        parse_expr(tokens)
            .flat_map(|(lhs, tokens)| {
                eat(tokens, Token::op).flat_map(|(op, tokens)| {
                    parse_expr(tokens)
                        .map(move |(rhs, tokens)| (Rc::new(Expr::Bin(lhs, op, rhs)), tokens))
                })
            })
            .union(
                // `expr ::= LIT`
                eat(tokens, Token::lit).map(|(x, tokens)| (Rc::new(Expr::Const(x)), tokens)),
            )
    };

    // Eager `LazySet` execution.
    {
        let mut parse_expr = memoize_eagerly(parse_expr);
        test(|tokens| parse_expr.call(tokens).clone());
    }
}

fn main() {
    eager();
    eprintln!();
    lazy_set();
}
