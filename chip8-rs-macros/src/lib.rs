use chip8_rs_common::{Address, Nibble};
use proc_macro::{Delimiter, TokenStream};
use syn::{
    parse::{discouraged::AnyDelimiter, Parse, ParseBuffer, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Error, Ident, LitInt, LitStr, Token,
};

enum EncodingType {
    Value(u16),
    Address(Address),
    Xk(Nibble),
    Xy(Nibble, Nibble),
    Xyn(Nibble),
    X(Nibble, u8),
}

impl Parse for EncodingType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?.to_string();

        let delimited_buffer;
        syn::parenthesized!(delimited_buffer in input);

        match ident.as_str() {
            "value" => {
                let delimited_lit: LitInt = delimited_buffer.parse()?;
                let delimited_int: u16 = delimited_lit.base10_parse()?;
                Ok(Self::Value(delimited_int))
            }
            "xk" => {
                // if let Some(nibble) = Nibble::new(delimited_int) {
                //     Ok(Self::Value(delimited_int))
                // } else {
                //     return Err(Error::new_spanned(
                //         &delimited_lit,
                //         "Literal out of range for nibble",
                //     ));
                // }

                todo!();
            }
            _ => todo!(),
        }
    }
}

struct InstructionDefinition {
    name: Ident,
    encoding_type: EncodingType,
}

impl Parse for InstructionDefinition {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![=>]>()?;
        let encoding_type: EncodingType = input.parse()?;

        Ok(Self {
            name,
            encoding_type,
        })
    }
}

struct InstructionDefinitions {
    defs: Vec<InstructionDefinition>,
}

impl Parse for InstructionDefinitions {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let defs = Punctuated::<InstructionDefinition, Token![,]>::parse_terminated(input)?;

        Ok(Self {
            defs: defs.into_iter().collect(),
        })
    }
}

#[proc_macro]
pub fn instructions(item: TokenStream) -> TokenStream {
    parse_macro_input!(item as InstructionDefinitions);

    Default::default()
}
