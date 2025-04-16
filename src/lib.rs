use syn::{
    parse_macro_input,
    parse_quote,
    punctuated::Punctuated,
    FnArg, GenericParam, ItemFn, PatType, Path, Token, Type, TypePath, WhereClause,
};
use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[proc_macro_attribute]
pub fn gen_may_cancel_future(attr: TokenStream, item: TokenStream) -> TokenStream {
    let prefix_args = parse_macro_input!(attr with Punctuated::<Path, Token![,]>::parse_terminated);
    let input_fn = parse_macro_input!(item as ItemFn);

    let prefix_ident = if prefix_args.len() == 1 {
        prefix_args.first().unwrap().get_ident().cloned().expect("Expected identifier as path")
    } else {
        panic!("Expected exactly one identifier as prefix");
    };

    // Check function is async
    if input_fn.sig.asyncness.is_none() {
        panic!("`#[gen_may_cancel_future]` can only be applied to async functions");
    }

    // Extract signature parts
    // let vis = &input_fn.vis;
    let fn_ident = &input_fn.sig.ident;
    let fn_generics = &input_fn.sig.generics;
    let Option::Some(where_clause) = &input_fn.sig.generics.where_clause else {
        panic!("Function must have where clause for generics");
    };
    let where_clause_no_cancel = {
        let mut preds_no_cancel = where_clause.predicates.clone();
        let take_len = preds_no_cancel.len().saturating_sub(1);
        preds_no_cancel = preds_no_cancel
            .into_pairs()
            .take(take_len)
            .collect();
        if !preds_no_cancel.is_empty() {
            WhereClause {
                where_token: where_clause.where_token,
                predicates: preds_no_cancel,
            }
        } else {
            // A dummy where clause
            parse_quote! {
                where 'static: 'static
            }
        }
    };

    let inputs = &input_fn.sig.inputs;
    let output = &input_fn.sig.output;

    // Extract lifetime
    let mut lifetimes = vec![];
    for param in &fn_generics.params {
        if let GenericParam::Lifetime(lt) = param {
            lifetimes.push(lt.lifetime.clone());
        }
    }
    if lifetimes.len() != 1 {
        panic!("Function must have exactly one named lifetime");
    }
    let lt = &lifetimes[0]; // e.g., 'f

    // Extract all generic type parameters
    let (generics_all, generics_no_cancel) = {
        let mut generics_all = vec![];
        let mut generics_no_cancel = vec![];
        for (i, param) in fn_generics.params.iter().enumerate() {
            if let GenericParam::Type(ty) = param {
                generics_all.push(ty.ident.clone());

                if i < fn_generics.params.len() - 1 {
                    generics_no_cancel.push(ty.ident.clone());
                }
                // Currently we don't have reliable check the type bound for the 
                // last parameter `C: TrCancellationToken`. We simply assume it is
                // the last one and always correct.
            }
        }
        if generics_all.is_empty() {
            panic!("Function must have at least one generic parameter");
        }
        (generics_all, generics_no_cancel)
    };

    // Extract inputs except cancel token
    let mut fields = vec![];
    let mut types = vec![];
    let mut args = vec![];

    let mut cancel_type = None;
    // let mut cancel_pat = None;

    for (i, input) in inputs.iter().enumerate() {
        match input {
            FnArg::Typed(PatType { pat, ty, .. }) => {
                let is_last = i == inputs.len() - 1;

                if is_last {
                    // Expect: Pin<&'f mut C>
                    if let Type::Path(TypePath { path, .. }) = &**ty {
                        if path.segments.last().unwrap().ident != "Pin" {
                            panic!("Last argument must be Pin<&'f mut C>");
                        }
                    }

                    cancel_type = Some(ty.clone());
                    // cancel_pat = Some(pat.clone());
                } else {
                    fields.push(ty.clone());
                    types.push(ty.clone());
                    args.push(pat.clone());
                }
            }
            _ => panic!("Unsupported argument format"),
        }
    }

    let async_struct = format_ident!("{}Async", prefix_ident);
    let future_struct = format_ident!("{}Future", prefix_ident);
    let state_struct = format_ident!("{}FutureState", prefix_ident);

    // Final generic types
    // let gen_params = quote! { #(#generics_all),* };
    // let gen_params_with_lt = quote! { #lt, #(#generics_all),* };
    let output_ty = match output {
        syn::ReturnType::Type(_, ty) => ty,
        _ => panic!("Expected function to return a value"),
    };

    let expanded = quote! {
        #input_fn

        pub struct #async_struct<#lt, #(#generics_no_cancel),*>(#(#fields),*)
        #where_clause_no_cancel;

        pub struct #future_struct<#lt, #(#generics_all),*,>
        #where_clause
        {
            params_: #async_struct<#lt, #(#generics_no_cancel),*>,
            cancel_: #cancel_type,
            fstate_: Option<#state_struct<#lt, #(#generics_all),*,>>,
        }

        struct #state_struct<#lt, #(#generics_all),*,>(#cancel_type)
        #where_clause;

        impl<#lt, #(#generics_no_cancel),*> ::core::future::IntoFuture for #async_struct<#lt, #(#generics_no_cancel),*>
        #where_clause_no_cancel
        {
            type IntoFuture = #future_struct<#lt, #(#generics_no_cancel),*, NonCancellableToken>;
            type Output = #output_ty;

            fn into_future(self) -> Self::IntoFuture {
                #future_struct {
                    params_: self,
                    cancel_: ::abs_sync::cancellation::NonCancellableToken::shared(),
                    fstate_: Option::None,
                }
            }
        }

        impl<#lt, #(#generics_no_cancel),*> ::abs_sync::cancellation::TrMayCancel<#lt> for #async_struct<#lt, #(#generics_no_cancel),*>
        #where_clause_no_cancel
        {
            type MayCancelOutput = #output_ty;

            fn may_cancel_with<'f, C>(
                self,
                cancel: ::core::pin::Pin<&'f mut C>,
            ) -> impl ::core::future::Future<Output = Self::MayCancelOutput>
            where
                C: ::abs_sync::cancellation::TrCancellationToken,
            {
                #future_struct {
                    params_: self,
                    cancel_: cancel,
                    fstate_: Option::None,
                }
            }
        }

        impl<#lt, #(#generics_all),*> ::core::future::Future for #future_struct<#lt, #(#generics_all),*>
        #where_clause
        {
            type Output = #output_ty;

            fn poll(
                self: ::core::pin::Pin<&mut Self>,
                cx: &mut ::core::task::Context<'_>,
            ) -> ::core::task::Poll<Self::Output> {
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
                        let state_pin = unsafe { ::core::pin::Pin::new_unchecked(state) };
                        break state_pin.poll(cx)
                    } else {
                        let state = BuffPeekInputFutureState::new(unsafe { 
                            ::core::pin::Pin::new_unchecked(this.as_mut())
                        });
                        let future = state();
                        let state_field_mut = unsafe { state_field_ptr.as_mut() };
                        *state_field_mut = Option::Some(future);
                    }
                }
            }
        }

        impl<#(#generics_all),*> ::core::ops::AsyncFnOnce<()> for #state_struct<'_, #(#generics_all),*>
        #where_clause
        {
            type Output = #output_ty;
            type CallOnceFuture = impl ::core::future::Future<Output = Self::Output>;

            extern "rust-call" fn async_call_once(self, _: ()) -> Self::CallOnceFuture {
                let f = unsafe { self.0.get_unchecked_mut() };
                let p = &mut f.params_;
                self::#fn_ident(#(&mut p.#args),*, f.cancel_)
            }
        }
    };

    TokenStream::from(expanded)
}
