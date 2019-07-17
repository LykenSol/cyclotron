use cyclotron::eager::Memoized as _;
use cyclotron::lazy_set::LazySet as _;

use std::collections::BTreeSet;

fn collatz_next(x: u64) -> u64 {
    if x % 2 == 0 {
        x / 2
    } else {
        3 * x + 1
    }
}

fn test(mut collatz_all: impl FnMut(u64) -> BTreeSet<u64>) {
    eprintln!("collatz_all(19) = {:?}", collatz_all(19));
    eprintln!("collatz_all(2) = {:?}", collatz_all(2));
}

fn eager() {
    use cyclotron::eager::memoize;

    let mut collatz_all = memoize(|collatz_all, x| -> BTreeSet<u64> {
        let next = collatz_next(x);
        let mut set = collatz_all.call(next).clone();
        set.insert(next);
        set
    });
    test(|x| collatz_all.call(x).clone());
}

fn lazy_set() {
    use cyclotron::lazy_set::{call, memoize_eagerly, one};

    let collatz_all = call;
    let collatz_all = |x| {
        let next = collatz_next(x);
        collatz_all(next).union(one(next))
    };

    // Eager `LazySet` execution.
    {
        let mut collatz_all = memoize_eagerly(collatz_all);
        test(|x| collatz_all.call(x).clone());
    }
}

fn main() {
    eager();
    lazy_set();
}
