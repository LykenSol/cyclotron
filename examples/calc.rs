use cyclotron::eager::{memoize, Memoized};

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

fn main() {
    let mut parse_expr = memoize(
        |parse_expr, tokens: &[Token]| -> BTreeSet<(Rc<Expr>, &[Token])> {
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
    let mut parse_and_eval = |tokens| {
        let parses = parse_expr.call(tokens);
        eprintln!("parse_expr({:?}) =", tokens);
        for (e, after) in parses {
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
