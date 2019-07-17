use std::collections::BTreeSet;
use std::fmt;
use std::hash::Hash;

// HACK(eddyb) sealed traits/enums to keep them from being used outside of this module.
mod sealed {
    pub enum Step<Req, A, R> {
        Request(Req, A),
        // FIXME(eddyb) use GATs to move this into a request.
        Fork(A, A),
        Return(R),
    }

    impl<Req, A, R> Step<Req, A, R> {
        fn map_cont<B>(self, f: impl Fn(A) -> B) -> Step<Req, B, R> {
            match self {
                Step::Request(req, a) => Step::Request(req, f(a)),
                Step::Fork(x, y) => Step::Fork(f(x), f(y)),
                Step::Return(r) => Step::Return(r),
            }
        }

        fn map_ret<S>(self, f: impl FnOnce(R) -> S) -> Step<Req, A, S> {
            match self {
                Step::Request(req, a) => Step::Request(req, a),
                Step::Fork(x, y) => Step::Fork(x, y),
                Step::Return(r) => Step::Return(f(r)),
            }
        }
    }

    pub trait LazySet<Req, Res>: Clone {
        type Item;
        fn step(self, res: Option<Res>) -> Step<Req, Self, Option<Self::Item>>;
    }

    #[derive(Clone)]
    pub struct One<T>(pub(super) T);

    impl<Req, Res, T: Clone> LazySet<Req, Res> for One<T> {
        type Item = T;
        fn step(self, res: Option<Res>) -> Step<Req, Self, Option<Self::Item>> {
            assert!(res.is_none());
            Step::Return(Some(self.0))
        }
    }

    impl<Req, Res, T: Clone> LazySet<Req, Res> for Option<T> {
        type Item = T;
        fn step(self, res: Option<Res>) -> Step<Req, Self, Option<Self::Item>> {
            assert!(res.is_none());
            Step::Return(self)
        }
    }

    #[derive(Clone)]
    pub enum Request<Req> {
        Start(Req),
        Complete,
    }

    impl<Req: Clone, Res> LazySet<Req, Res> for Request<Req> {
        type Item = Res;
        fn step(self, res: Option<Res>) -> Step<Req, Self, Option<Self::Item>> {
            match self {
                Request::Start(req) => {
                    assert!(res.is_none());
                    Step::Request(req, Request::Complete)
                }
                Request::Complete => Step::Return(Some(res.unwrap())),
            }
        }
    }

    #[derive(Clone)]
    pub enum Union<A, B> {
        Start(A, B),
        JustA(A),
        JustB(B),
    }

    impl<Req, Res, A, B, T> LazySet<Req, Res> for Union<A, B>
    where
        A: LazySet<Req, Res, Item = T>,
        B: LazySet<Req, Res, Item = T>,
    {
        type Item = T;
        fn step(self, res: Option<Res>) -> Step<Req, Self, Option<Self::Item>> {
            match self {
                Union::Start(a, b) => {
                    assert!(res.is_none());
                    Step::Fork(Union::JustA(a), Union::JustB(b))
                }
                Union::JustA(a) => a.step(res).map_cont(Union::JustA),
                Union::JustB(b) => b.step(res).map_cont(Union::JustB),
            }
        }
    }

    #[derive(Clone)]
    pub struct Map<A, F>(pub(super) A, pub(super) F);

    impl<Req, Res, A, F, T> LazySet<Req, Res> for Map<A, F>
    where
        A: LazySet<Req, Res>,
        F: FnOnce(A::Item) -> T + Clone,
    {
        type Item = T;
        fn step(self, res: Option<Res>) -> Step<Req, Self, Option<Self::Item>> {
            let Map(a, f) = self;
            a.step(res)
                .map_cont(|a| Map(a, f.clone()))
                .map_ret(|r| r.map(f))
        }
    }

    #[derive(Clone)]
    pub enum FlatMap<A, B, F> {
        Start(A, F),
        Then(B),
    }

    impl<Req, Res, A, B, F> LazySet<Req, Res> for FlatMap<A, B, F>
    where
        A: LazySet<Req, Res>,
        B: LazySet<Req, Res>,
        F: FnOnce(A::Item) -> B + Clone,
    {
        type Item = B::Item;
        fn step(self, res: Option<Res>) -> Step<Req, Self, Option<Self::Item>> {
            match self {
                FlatMap::Start(a, f) => match a.step(res) {
                    Step::Request(req, a) => Step::Request(req, FlatMap::Start(a, f)),
                    Step::Fork(x, y) => {
                        Step::Fork(FlatMap::Start(x, f.clone()), FlatMap::Start(y, f))
                    }
                    Step::Return(None) => Step::Return(None),
                    Step::Return(Some(r)) => f(r).step(None).map_cont(FlatMap::Then),
                },
                FlatMap::Then(b) => b.step(res).map_cont(FlatMap::Then),
            }
        }
    }
}

// NOTE(eddyb) this would normally be named `LazySetExt`, but we want to allow
// users to use this trait in bounds, without exposing the sealed one.
pub trait LazySet<Req, Res>: sealed::LazySet<Req, Res> {
    fn union<B: LazySet<Req, Res, Item = Self::Item>>(self, b: B) -> sealed::Union<Self, B> {
        sealed::Union::Start(self, b)
    }

    fn map<F: FnOnce(Self::Item) -> T, T>(self, f: F) -> sealed::Map<Self, F> {
        sealed::Map(self, f)
    }

    fn flat_map<B: LazySet<Req, Res>, F: FnOnce(Self::Item) -> B>(
        self,
        f: F,
    ) -> sealed::FlatMap<Self, B, F> {
        sealed::FlatMap::Start(self, f)
    }

    fn run(
        self,
        handle: &mut impl FnMut(Req, Self) -> BTreeSet<Res>,
        res: Option<Res>,
    ) -> BTreeSet<Self::Item>
    where
        Res: Ord,
        Self::Item: Ord,
    {
        let mut output = BTreeSet::new();
        match self.step(res) {
            sealed::Step::Request(req, a) => {
                for res in handle(req, a.clone()) {
                    output.extend(a.clone().run(handle, Some(res)));
                }
            }
            sealed::Step::Fork(a, b) => {
                output.extend(a.run(handle, None));
                output.extend(b.run(handle, None));
            }
            sealed::Step::Return(v) => output.extend(v),
        }
        output
    }
}

impl<Req, Res, A: sealed::LazySet<Req, Res>> LazySet<Req, Res> for A {}

pub fn one<T>(x: T) -> sealed::One<T> {
    sealed::One(x)
}

pub fn request<Req>(req: Req) -> sealed::Request<Req> {
    sealed::Request::Start(req)
}

#[derive(Clone)]
pub struct Call<T>(pub T);

pub fn call<T>(x: T) -> sealed::Request<Call<T>> {
    request(Call(x))
}

// FIXME(eddyb) this should be in `LazyExt`, but `-> impl Trait`
// doesn't work in traits yet, move it there whenever that changes.
pub fn to_eager<K, V, A>(
    lazy_f: impl FnOnce(K) -> A + Clone,
) -> impl FnOnce(&mut dyn FnMut(K) -> BTreeSet<V>, K) -> BTreeSet<V> + Clone
where
    K: Copy + Eq + Hash + fmt::Debug,
    V: Clone + Ord + fmt::Debug,
    A: LazySet<Call<K>, V, Item = V>,
{
    move |f, k| lazy_f(k).run(&mut |Call(k), _| f(k), None)
}

// FIXME(eddyb) figure out module hierarchy here.
pub mod depth_first {
    use super::*;

    use std::collections::{hash_map::Entry, HashMap};

    struct CallState<K, V, A> {
        // FIXME(eddyb) closures can't be Ord or Hash today.
        // returns: BTreeSet<(K, A)>,
        returns: Vec<(K, A)>,
        results: BTreeSet<V>,
    }

    struct MemoState<K, V, A, F> {
        calls: HashMap<K, CallState<K, V, A>>,
        f: F,
    }

    pub fn memoize<K, V, A>(f: impl FnOnce(K) -> A + Clone) -> impl FnMut(K) -> BTreeSet<V>
    where
        K: Copy + Ord + Hash + fmt::Debug,
        V: Clone + Ord + fmt::Debug,
        A: LazySet<Call<K>, V, Item = V>,
    {
        let mut state = MemoState {
            calls: HashMap::new(),
            f,
        };
        move |k| match state.calls.entry(k) {
            // `f(k)` complete (while technically `MemoState` could allow partial
            // results to be exposed, the only entry-point into `state` is this
            // closure, which is `FnMut`, disallowing reentrance).
            Entry::Occupied(entry) => entry.get().results.clone(),

            // `f(k)` never attempted before, we have to run it now.
            Entry::Vacant(entry) => {
                entry.insert(CallState {
                    returns: Default::default(),
                    results: Default::default(),
                });
                let f = state.f.clone();
                state.run(k, f(k), None);
                state.calls[&k].results.clone()
            }
        }
    }

    impl<K, V, A, F> MemoState<K, V, A, F>
    where
        K: Copy + Eq + Hash + fmt::Debug,
        V: Clone + Ord + fmt::Debug,
        A: LazySet<Call<K>, V, Item = V>,
        F: FnOnce(K) -> A + Clone,
    {
        // FIXME(eddyb) try to make `LazySet::run` flexible enough to use it here.
        fn run(&mut self, current: K, a: A, res: Option<V>) {
            match a.step(res) {
                // FIXME(eddyb) deduplicate some of this.
                sealed::Step::Request(Call(k), a) => match self.calls.entry(k) {
                    // `f(k)` in progress or completed.
                    Entry::Occupied(entry) => {
                        let call = entry.into_mut();
                        // FIXME(eddyb) closures can't be Ord or Hash today.
                        // if call.returns.insert((current, a.clone()))
                        call.returns.push((current, a.clone()));
                        {
                            // FIXME(eddyb) try to avoid cloning the set.
                            for v in call.results.clone() {
                                self.run(current, a.clone(), Some(v));
                            }
                        }
                    }

                    // `f(k)` never attempted before, we have to start it now.
                    Entry::Vacant(entry) => {
                        let call = entry.insert(CallState {
                            returns: Default::default(),
                            results: Default::default(),
                        });
                        // FIXME(eddyb) closures can't be Ord or Hash today.
                        // call.returns.insert((current, a.clone()));
                        call.returns.push((current, a.clone()));
                        let f = self.f.clone();
                        self.run(k, f(k), None);
                    }
                },
                sealed::Step::Fork(a, b) => {
                    self.run(current, a, None);
                    self.run(current, b, None);
                }
                sealed::Step::Return(None) => {}
                sealed::Step::Return(Some(v)) => {
                    let call = self.calls.get_mut(&current).unwrap();
                    if call.results.insert(v.clone()) {
                        // FIXME(eddyb) try to avoid cloning the set.
                        for (caller, ret) in call.returns.clone() {
                            self.run(caller, ret, Some(v.clone()));
                        }
                    }
                }
            }
        }
    }
}
