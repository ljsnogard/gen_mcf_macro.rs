// src/lib.rs
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, FnArg, GenericParam, Ident, ItemFn, Lifetime, Pat, PatIdent,
    PatType, ReturnType, //Type, TypeReference,
};

#[proc_macro_attribute]
pub fn gen_may_cancel_future(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);

    // Support #[gen_may_cancel_future(Prefix)] style
    let prefix_ident = if attr.is_empty() {
        None
    } else {
        Some(parse_macro_input!(attr as Ident))
    };

    expand_gen_may_cancel_future(input_fn, prefix_ident).into()
}

fn expand_gen_may_cancel_future(input_fn: ItemFn, prefix: Option<Ident>) -> proc_macro2::TokenStream {
    let fn_name = &input_fn.sig.ident;
    // let fn_vis = &input_fn.vis;
    let fn_generics = &input_fn.sig.generics;
    let (impl_generics, ty_generics, where_clause) = fn_generics.split_for_impl();

    let output_ty = match &input_fn.sig.output {
        ReturnType::Type(_, ty) => quote! { #ty },
        _ => panic!("Function must return a value"),
    };

    let lifetimes: Vec<_> = fn_generics.params.iter().filter_map(|p| match p {
        GenericParam::Lifetime(l) => Some(&l.lifetime),
        _ => None,
    }).collect();
    let primary_lt: &Lifetime = lifetimes.get(0).expect("Expected at least one lifetime");

    let base = prefix.clone().unwrap_or_else(|| fn_name.clone());
    let struct_name = format_ident!("{}Async", base);
    let fut_name = format_ident!("{}Future", base);
    let state_name = format_ident!("{}FutureState", base);

    let mut fields = Vec::new();
    let mut field_idents = Vec::new();
    let mut args = Vec::new();
    let mut cancel_ident: Option<Ident> = None;

    for arg in &input_fn.sig.inputs {
        match arg {
            FnArg::Typed(PatType { pat, ty, .. }) => {
                let ident = match &**pat {
                    Pat::Ident(PatIdent { ident, .. }) => ident.clone(),
                    _ => panic!("Unsupported pattern"),
                };

                let ty_string = quote!(#ty).to_string();
                if ty_string.contains("Pin") && ty_string.contains("mut") {
                    cancel_ident = Some(ident.clone());
                    args.push(quote!(#ident));
                    continue;
                }

                fields.push(quote! { pub #ident: #ty });
                field_idents.push(ident.clone());
                args.push(quote!(this.#ident));
            }
            _ => panic!("Unsupported argument"),
        }
    }

    let cancel_ident = cancel_ident.expect("Expected a cancel token Pin<&mut C>");

    quote! {
        #input_fn

        pub struct #struct_name #impl_generics #where_clause {
            #(#fields,)*
        }

        pub struct #fut_name<'a, #ty_generics> #where_clause {
            #(#fields,)*
            #cancel_ident: ::core::pin::Pin<&#primary_lt mut C>,
            state: Option<#state_name<'a, #ty_generics>>,
        }

        struct #state_name<'a, #ty_generics> #where_clause (
            ::core::pin::Pin<&'a mut #fut_name<'a, C, #ty_generics>>
        );

        impl<'a, C, #ty_generics> #state_name<'a, C, #ty_generics> #where_clause {
            pub fn call_once(self) -> impl ::core::future::Future<Output = #output_ty> + 'a {
                let this = self.0.get_mut();
                #fn_name(#(#args,)* this.#cancel_ident.as_mut())
            }
        }

        impl<'a, C, #ty_generics> ::core::future::Future for #fut_name<'a, C, #ty_generics>
        #where_clause
        where
            C: TrCancellationToken,
        {
            type Output = #output_ty;

            fn poll(mut self: ::core::pin::Pin<&mut Self>, cx: &mut ::core::task::Context<'_>) -> ::core::task::Poll<Self::Output> {
                if self.state.is_none() {
                    let state = #state_name(::core::pin::Pin::new(self));
                    self.state = Some(Box::pin(state.call_once()));
                }
                ::core::pin::Pin::new(self.state.as_mut().unwrap()).poll(cx)
            }
        }

        impl<'a, #ty_generics> ::core::future::IntoFuture for #struct_name<'a, #ty_generics>
        #where_clause
        {
            type IntoFuture = #fut_name<'a, NonCancellableToken, #ty_generics>;
            type Output = #output_ty;

            fn into_future(self) -> Self::IntoFuture {
                #fut_name {
                    #(#field_idents: self.#field_idents,)*
                    #cancel_ident: NonCancellableToken::shared(),
                    state: None,
                }
            }
        }

        impl<'a, #ty_generics> TrMayCancel<'a> for #struct_name<'a, #ty_generics>
        #where_clause
        {
            type MayCancelOutput = #output_ty;

            fn may_cancel_with<'f, C>(self, cancel: ::core::pin::Pin<&'f mut C>) -> impl ::core::future::Future<Output = Self::MayCancelOutput>
            where
                C: TrCancellationToken + 'f,
            {
                #fut_name {
                    #(#field_idents: self.#field_idents,)*
                    #cancel_ident: cancel,
                    state: None,
                }
            }
        }
    }
}
