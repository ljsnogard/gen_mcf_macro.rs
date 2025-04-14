extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn, Ident, FnArg, PatType, Type};
use syn::spanned::Spanned;

#[proc_macro_attribute]
pub fn gen_may_cancel_future(attr: TokenStream, input: TokenStream) -> TokenStream {
    let struct_name = parse_macro_input!(attr as Ident);
    let input_fn = parse_macro_input!(input as ItemFn);

    let gen_code = generate_code(struct_name, input_fn).unwrap_or_else(|e| e.to_compile_error());
    TokenStream::from(gen_code)
}

fn generate_code(struct_name: Ident, input_fn: ItemFn) -> Result<proc_macro2::TokenStream, syn::Error> {
    // 解析原始函数
    let fn_vis = &input_fn.vis;
    let fn_name = &input_fn.sig.ident;
    let fn_asyncness = input_fn.sig.asyncness.as_ref().ok_or_else(|| {
        syn::Error::new(input_fn.sig.span(), "function must be async")
    })?;
    
    // 解析参数（排除最后一个cancel参数）
    let params: Vec<&FnArg> = input_fn.sig.inputs.iter().collect();
    let (async_params, cancel_param) = params
        .split_at(params.len() - 1);
        // .ok_or_else(|| syn::Error::new(input_fn.sig.span(), "missing cancel parameter"))?;
    
    // 验证最后一个参数是否是Pin<&mut C>
    let cancel_type = match cancel_param[0] {
        FnArg::Typed(PatType { ty, .. }) => match ty.deref() {
            Type::Path(p) => p.path.segments.last().unwrap().ident.clone(),
            _ => return Err(syn::Error::new(ty.span(), "invalid cancel parameter type"))
        },
        _ => return Err(syn::Error::new(cancel_param[0].span(), "expected typed parameter"))
    };

    // 生成结构体字段
    let async_fields = async_params.iter().map(|arg| {
        match arg {
            FnArg::Typed(PatType { pat, ty, .. }) => {
                let ident = match pat.deref() {
                    syn::Pat::Ident(i) => &i.ident,
                    _ => return Err(syn::Error::new(pat.span(), "unsupported pattern"))
                };
                Ok(quote! { #ident: #ty })
            }
            _ => Err(syn::Error::new(arg.span(), "unsupported parameter type"))
        }
    }).collect::<Result<Vec<_>, _>>()?;

    // 生成泛型参数
    let generics = &input_fn.sig.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // 生成结构体名称
    let async_struct = Ident::new(&format!("{}Async", struct_name), struct_name.span());
    let future_struct = Ident::new(&format!("{}Future", struct_name), struct_name.span());
    let state_struct = Ident::new(&format!("{}FutureState", struct_name), struct_name.span());

    Ok(quote! {
        #input_fn

        #fn_vis struct #async_struct<'a, #generics> {
            #(#async_fields),*
        }

        #fn_vis struct #future_struct<'a, C, #generics> {
            #(#async_fields),*,
            s: Option<#state_struct<'a, C, #generics>>,
        }

        struct #state_struct<'a, C, #generics>(core::pin::Pin<&'a mut #future_struct<'a, C, #generics>>);

        impl #impl_generics core::future::IntoFuture for #async_struct<'a, #generics> #where_clause {
            type IntoFuture = #future_struct<'a, NonCancellableToken, #generics>;
            
            fn into_future(self) -> Self::IntoFuture {
                #future_struct {
                    #(self.#async_fields),*,
                    s: None,
                }
            }
        }

        impl<'a, C, #generics> core::future::Future for #future_struct<'a, C, #generics> 
        where
            C: TrCancellationToken,
            #where_clause
        {
            type Output = KnownType;

            fn poll(
                self: core::pin::Pin<&mut Self>,
                cx: &mut core::task::Context<'_>
            ) -> core::task::Poll<Self::Output> {
                let this = unsafe { self.get_unchecked_mut() };
                if this.s.is_none() {
                    let state = #state_struct(unsafe { core::pin::Pin::new_unchecked(this) });
                    this.s = Some(state);
                }
                
                let state = this.s.as_mut().unwrap();
                let fut = unsafe { core::pin::Pin::new_unchecked(&mut state.0) };
                <#future_struct as core::future::Future>::poll(fut, cx)
            }
        }

        impl<'a, C, #generics> core::ops::AsyncFnOnce<()> for #state_struct<'a, C, #generics> 
        where
            C: TrCancellationToken,
            #where_clause
        {
            type Output = KnownType;
            type CallOnceFuture = impl core::future::Future<Output = Self::Output>;

            fn call_once(self, _: ()) -> Self::CallOnceFuture {
                async move {
                    let #future_struct { #async_fields, s, .. } = self.0;
                    #fn_name(#async_fields, cancel).await
                }
            }
        }
    })
}