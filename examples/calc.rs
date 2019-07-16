use cyclotron::eager::Memoized as _;

use std::collections::BTreeSet;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum Token {
    Lit(i32),
    Op(char),
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
            for (lhs, after_lhs) in parse_expr.call(tokens).clone() {
                if let Some(&Token::Op(op)) = after_lhs.get(0) {
                    for (rhs, after_rhs) in parse_expr.call(&after_lhs[1..]).clone() {
                        parses
                            .insert((Rc::new(Expr::Bin(lhs.clone(), op, rhs.clone())), after_rhs));
                    }
                }
            }

            // `expr ::= LIT`
            if let Some(&Token::Lit(x)) = tokens.get(0) {
                parses.insert((Rc::new(Expr::Const(x)), &tokens[1..]));
            }

            parses
        },
    );
    test(|tokens| parse_expr.call(tokens).clone());
}

fn lazy_set_to_eager<'a>() {
    use cyclotron::lazy_set::{call as parse_expr, once, to_eager, LazySet};

    let mut parse_expr = to_eager(|tokens: &'a [Token]| {
        // `expr ::= expr OP expr`
        parse_expr(tokens)
            .filter_map(
                |(lhs, after_lhs): (Rc<Expr>, &'a [Token])| match after_lhs.get(0) {
                    Some(&Token::Op(op)) => Some((lhs, op, &after_lhs[1..])),
                    _ => None,
                },
            )
            .flat_map(|(lhs, op, after_op)| {
                parse_expr(after_op).map(move |(rhs, after_rhs)| {
                    (Rc::new(Expr::Bin(lhs.clone(), op, rhs)), after_rhs)
                })
            })
            .union(
                // `expr ::= LIT`
                // FIXME(eddyb) inference fails for `filter_map`'s `CallK` and `CallV`
                // for some reasons. Possible fixes:
                //   * replace this `once(...).filter_map(...)` hack with an `Option` `LazySet`
                //   * make `CallK` and `CallV` associated items
                LazySet::<&'a [Token], (Rc<Expr>, &'a [Token])>::filter_map(
                    once(tokens),
                    |tokens| match tokens.get(0) {
                        Some(&Token::Lit(x)) => Some((Rc::new(Expr::Const(x)), &tokens[1..])),
                        _ => None,
                    },
                ),
            )
    });
    test(|tokens| parse_expr.call(tokens).clone());
}

fn main() {
    eager();
    eprintln!();
    lazy_set_to_eager();
}
