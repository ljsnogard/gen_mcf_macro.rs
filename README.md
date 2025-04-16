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

pub struct DoThingAsync<'a, A, B>(
    &'a mut A,
    &'a mut B,
    usize,
    Result<A, B>,
)
where
    A: Send,
    B: Sync;

pub struct DoThingFuture<'a, A, B, C>
where
    A: Send,
    B: Sync,
    C: TrCancellationToken,
{
    params_: DoThingAsync<'a, A, B>,
    cancel_: Pin<&'a mut C>,
    fstate_: Option<DothingFutureState<'a, A, B, C>>,
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
            params_: self,
            cancel_: NonCancellableToken::shared(), // when no cancellation specified, use `NonCancellableToken`
            fstate_: Option::None,
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
            params_: self,
            cancel_: cancel, // use the specified cancel token
            fstate_: Option::None,
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
            let p = self.get_unchecked_mut();
            ::core::ptr::NonNull::new_unchecked(p)
        };
        loop {
            let mut state_field_ptr = unsafe {
                let ptr = &mut this.as_mut().fstate_;
                ::core::ptr::NonNull::new_unchecked(ptr)
            };
            let opt_state = unsafe { state_field_ptr.as_mut() };
            if let Option::Some(state) = opt_state {
                let state_field_pin = unsafe { Pin::new_unchecked(state) };
                break state_field_pin.poll(cx)
            } else {
                let state = DoThingFutureState(unsafe { Pin::new_unchecked(this.as_mut()) });
                let future = state();
                let state_field_mut = unsafe { state_field_ptr.as_mut() };
                *state_field_mut = Option::Some(future);
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
    extern "rust-call" fn async_call_once(self, _: ()) -> Self::CallOnceFuture {
        let f = unsafe { self.0.get_unchecked_mut() };
        let a = &mut f.async_args_;
        self::do_thing_async(a.0, a.1, a.2, a.3, f.pin_cancel_)
    }
}

```