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
pub fn memoize_by_bruteforce<K, V, F, A>(
    lazy_f: F,
) -> impl crate::bruteforce::Memoized<K, Value = BTreeSet<V>>
where
    K: Copy + Eq + Hash + fmt::Debug,
    V: Clone + Ord + fmt::Debug,
    F: FnOnce(K) -> A + Clone,
    A: LazySet<Call<K>, V, Item = V>,
{
    crate::bruteforce::memoize(move |f, k| -> BTreeSet<V> {
        lazy_f.clone()(k).run(&mut |Call(k), _| f.call(k), None)
    })
}
