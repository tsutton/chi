extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[proc_macro]
pub fn gen_enum(_: TokenStream) -> TokenStream {
    let mut variants = Vec::new();
    for_each_combo(|t, op, is_path| {
        let ident = enum_ident(t, op, is_path);
        let tuple_body = match (is_path, t) {
            (true, _) => quote! {String},
            (false, CompareType::String | CompareType::Timestamp) => quote! {String},
            (false, CompareType::Numeric) => quote! { ::serde_json::Value},
        };
        variants.push(quote! {
            #ident (#tuple_body)
        });
    });
    #[rustfmt::skip]
    let out = quote! {
    pub enum DataTestExpression{
        #(#variants),*
    }
    };
    out.into()
}

#[proc_macro]
pub fn choice_match(_: TokenStream) -> TokenStream {
    let mut branches = Vec::new();
    for_each_combo(|t, op, is_path| branches.push(branch_arm(t, op, is_path)));
    // TODO add non-auto branches:
    // - IsPresent
    // - Is{Null, Boolean, Timestamp, String, Numeric}
    // - StringMatches
    let out = quote! {
    match expr {
    #(#branches),*
    }
    };
    out.into()
}

fn for_each_combo<T>(mut f: T)
where
    T: FnMut(&CompareType, &CompareOp, bool),
{
    for t in [
        CompareType::String,
        CompareType::Timestamp,
        CompareType::Numeric,
    ] {
        for op in [
            CompareOp::Equals,
            CompareOp::LessThan,
            CompareOp::LessThanEquals,
            CompareOp::GreaterThan,
            CompareOp::GreaterThanEquals,
        ] {
            for is_path in [true, false] {
                f(&t, &op, is_path);
            }
        }
    }
}

fn enum_ident(t: &CompareType, op: &CompareOp, is_path: bool) -> proc_macro2::Ident {
    if is_path {
        format_ident!("{}{}Path", t, op)
    } else {
        format_ident!("{}{}", t, op)
    }
}

// assumptions: we have input and variable in scope
fn branch_arm(t: &CompareType, op: &CompareOp, is_path: bool) -> proc_macro2::TokenStream {
    let ident = enum_ident(t, op, is_path);
    let retrieve_stmt = if is_path {
        // TODO(compliance) if a the path in SomethingPath fails to match, is it an error or simply false?
        // For now, doing false
        let to_string = match t {
            CompareType::String | CompareType::Timestamp => {
                quote! {
                            let right_hand_side = match right_hand_side {
                            ::serde_json::Value::String(value) => value,
                            _ => return Ok(false),
                            };
                }
            }
            CompareType::Numeric => quote! {},
        };
        #[rustfmt::skip]
        let extract = quote! {
            let right_hand_side = select_one(input, &right_hand_side);
            let right_hand_side = match right_hand_side {
		Err(_) => return Ok(false),
		Ok(v) => v,
            };
        };
        quote! {
            #extract
            #to_string
        }
    } else {
        quote! {}
    };
    // sets up left_hand_side from input and variable as either:
    // - the String inside a serde_json::Value::String(String) for t String and Timestamp
    // - the f64 made from the Number for Numeric and serde_json::Value::Number(Number)
    // retuning Ok(false) if the variant was wrong and using ? if the path selection fails
    // and returning Ok(false) if conversion from Number to f64 fails
    // TODO(compliance) if the path for variable fails to match, is it an error or simply false?
    let typecheck_and_order_stmt = match t {
        CompareType::String => {
            #[rustfmt::skip]
            quote! {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
		let order = left_hand_side.cmp(&right_hand_side);
            }
        }
        CompareType::Timestamp => {
            #[rustfmt::skip]
            quote! {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::String(value) => value,
                    _ => return Ok(false),
                };
		// TODO check that LHS and RHS are in timestamp format
                let order = left_hand_side.cmp(&right_hand_side);
            }
        }
        CompareType::Numeric => {
            #[rustfmt::skip]
            quote! {
                let left_hand_side = match select_one(input, variable)? {
                    ::serde_json::Value::Number(value) => match value.as_f64(){
			None => return Ok(false),
			Some(f) => f,
		    },
                    _ => return Ok(false),
                };
		let right_hand_side = match right_hand_side {
                    ::serde_json::Value::Number(r) => match r.as_f64(){
			None => return Ok(false),
			Some(f) => f,
		    },
		    _ => return Ok(false),
		};
                let order = match left_hand_side.partial_cmp(&right_hand_side){
		    None => return Ok(false),
		    Some(o) => o,
		};
            }
        }
    };
    let compare_stmt = match op {
        CompareOp::Equals => quote! {
            Ok(order.is_eq())
        },
        CompareOp::LessThan => quote! {
            Ok(order.is_lt())
        },
        CompareOp::LessThanEquals => quote! {
            Ok(order.is_le())
        },
        CompareOp::GreaterThan => quote! {
            Ok(order.is_gt())
        },
        CompareOp::GreaterThanEquals => quote! {
            Ok(order.is_ge())
        },
    };
    quote! {
    DataTestExpression::#ident(right_hand_side) => {
        #retrieve_stmt
        #typecheck_and_order_stmt
        #compare_stmt
    }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum CompareType {
    String,
    Timestamp,
    Numeric,
}

impl quote::IdentFragment for CompareOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(PartialEq, Eq, Debug)]
enum CompareOp {
    Equals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

impl quote::IdentFragment for CompareType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
