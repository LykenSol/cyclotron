use cyclotron::eager::Memoized as _;

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

fn lazy_set_to_eager() {
    use cyclotron::lazy_set::{call as collatz_all, once, to_eager, LazySet as _};

    let mut collatz_all = to_eager(|x| {
        let next = collatz_next(x);
        collatz_all(next).union(once(next))
    });
    test(|x| collatz_all.call(x).clone());
}

fn main() {
    eager();
    lazy_set_to_eager();
}
