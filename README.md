## Usage

```rust

use abs_sync::cancellation::{NonCancellableToken, TrMayCancel, TrCancellationToken};

#[gen_may_cancel_future(DoThing)]
async fn do_thing_async<'f, A, B, C>(
    a: &'f mut A,
    b: &'f mut B,
    l: usize,
    x: Result<A, B>,
    cancel: Pin<&'f mut C>,
) -> R
where
    A: Send,
    B: Sync,
    C: TrCancellationToken,
{
    // ...
}

```

Which expands to codes:

```rust

pub struct DoThingAsync<'a, A, B>
where
    A: Send,
    B: Sync,
{
    a: &'a mut A,
    b: &'a mut B,
    l: usize,
    x: Result<A, B>,
}

pub struct DoThingFuture<'a, A, B, C>
where
    A: Send,
    B: Sync,
    C: TrCancellationToken,
{
    a: &'a mut A,
    b: &'a mut B,
    l: usize,
    x: Result<A, B>,
    cancel: Pin<&'a mut C>,
    s: Option<DothingFutureState<'a, C, A, B>>,
}

struct DothingFutureState<'a, C, A, B>(Pin<&'a mut DoThingFuture<'a, A, B, C>>)
where
    A: Send,
    B: Sync,
    C: TrCancellationToken;

impl<'a, A, B> IntoFuture for DoThingAsync<'a, A, B>
where
    A: Send,
    B: Sync,
{
    type IntoFuture = DoThingFuture<'a, A, B, NonCancellableToken>;
    type Output = R;

    fn into_future(self) -> Self::IntoFuture {
        DoThingFuture {
            a: self.a,
            b: self.b,
            l: self.l,
            x: self.x,
            cancel: NonCancellableToken::shared(), // when no cancellation specified, use `NonCancellableToken`
            s: Option::None,
        }
    }
}

impl<'a, A, B> TrMayCancel<'a> for DoThingAsync<'a, A, B>
where
    A: Send,
    B: Sync,
{
    type MayCancelOutput = R;

    fn may_cancel_with<'f, C>(
        self,
        cancel: Pin<&'f mut C>,
    ) -> impl Future<Output = Self::MayCancelOutput>
    where
        C: TrCancellationToken,
    {
                DoThingFuture {
            a: self.a,
            b: self.b,
            l: self.l,
            x: self.x,
            cancel: cancel, // use the specified cancel token
            s: Option::None,
        }
    }
}

impl<'a, A, B, C> Future for DoThingFuture<'a, A, B, C>
where
    A: Send,
    B: Sync,
    C: TrCancellationToken,
{
    type Output = R;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = unsafe {
            let p = self.as_mut().get_unchecked_mut();
            NonNull::new_unchecked(p)
        };
        loop {
            let mut p = unsafe {
                let ptr = &mut this.as_mut().future_;
                NonNull::new_unchecked(ptr)
            };
            let opt_f = unsafe { p.as_mut() };
            if let Option::Some(f) = opt_f {
                let f_pinned = unsafe { Pin::new_unchecked(f) };
                break f_pinned.poll(cx)
            } else {
                let h = FutImpl::new(unsafe { 
                    Pin::new_unchecked(this.as_mut())
                });
                let f = h();
                let opt = unsafe { p.as_mut() };
                *opt = Option::Some(f);
            }
        }
    }
}

impl<'a, A, B, C> AsyncFnOnce<()> for DoThingFutureState<'a, A, B, C>
where
    A: Send,
    B: Sync,
    C: TrCancellationToken,
{
    type CallOnceFuture = impl Future<Output = Self::Output>;
    type Output = R;

    #[inline]
    extern "rust-call" fn async_call_once(
        self,
        _: (),
    ) -> Self::CallOnceFuture {
        let f = unsafe { self.0.get_unchecked_mut() };
        Self::call_once(
            f.a,
            f.b,
            f.l,
            f.x,
            f.cancel,
        )
    }
}

impl<'a, A, B, C> DoThingFutureState<'a, A, B, C>
where
    A: Send,
    B: Sync,
    C: TrCancellationToken,
{
    async fn call_once(
        a: &'f mut A,
        b: &'f mut B,
        l: usize,
        x: Result<A, B>,
        cancel: Pin<&'f mut C>,
    ) -> R {
        self::do_thing_async(a, b, l, x, cancel).await
    }
}

```