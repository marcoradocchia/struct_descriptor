use proc_macro::TokenStream;
use quote::quote;
use std::fmt::Display;
use syn::{
    parse_macro_input, spanned::Spanned, AttrStyle, Attribute, Data, DeriveInput, Expr, Field,
    Fields, Ident, Lit, LitStr, Meta,
};

trait OuterAttribute {
    fn is_ident(&self, ident: &str) -> bool;

    fn is_doc(&self) -> bool {
        self.is_ident("doc")
    }

    fn is_ignore(&self) -> bool {
        self.is_ident("ignore_field")
    }

    fn get_doc(&self) -> Option<&Lit>;
}

impl OuterAttribute for Attribute {
    fn is_ident(&self, ident: &str) -> bool {
        self.style == AttrStyle::Outer
            && self
                .path()
                .get_ident()
                .is_some_and(|ident_token| ident_token == ident)
    }

    fn get_doc(&self) -> Option<&Lit> {
        if !self.is_doc() {
            return None;
        }

        if let Meta::NameValue(ref meta_name_value) = self.meta {
            if let Expr::Lit(ref expr_lit) = meta_name_value.value {
                return Some(&expr_lit.lit);
            }
        }

        None
    }
}

#[derive(Debug)]
struct CompileError;

impl CompileError {
    fn new<S, E>(span: S, error: E) -> TokenStream
    where
        S: Spanned,
        E: Display,
    {
        syn::Error::new(span.span(), error)
            .to_compile_error()
            .into()
    }
}

#[derive(Debug, Clone, Copy)]
enum InvalidType {
    Enum,
    Union,
}

impl Display for InvalidType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`Describe` can only be derived on named field structs, found {}",
            match self {
                InvalidType::Enum => "enum",
                InvalidType::Union => "union",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
enum InvalidFieldType {
    Tuple,
    Unit,
}

impl Display for InvalidFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`Describe` can only be derived on named field structs, found {} struct",
            match self {
                InvalidFieldType::Tuple => "tuple",
                InvalidFieldType::Unit => "unit",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
enum FieldError {
    MissingIdent,
    MissingDoc,
}

impl Display for FieldError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`Describe` can only be derived on named and documented field structs: {}",
            match self {
                FieldError::MissingIdent => "missing field ident",
                FieldError::MissingDoc => "missing field documentation",
            }
        )
    }
}

#[derive(Debug)]
struct StructField<'ast> {
    ident: &'ast Ident,
    ignore: bool,
    doc: &'ast LitStr,
}

impl<'field> TryFrom<&'field Field> for StructField<'field> {
    type Error = TokenStream;

    fn try_from(field: &'field Field) -> Result<Self, Self::Error> {
        let mut ignore = false;
        let mut doc = None;

        for attr in &field.attrs {
            if attr.is_ignore() {
                ignore = true;
            } else if let Some(Lit::Str(doc_str)) = attr.get_doc() {
                doc = Some(doc_str);
            }
        }

        let ident = field
            .ident
            .as_ref()
            .ok_or_else(|| CompileError::new(field, FieldError::MissingIdent))?;
        let doc = doc.ok_or_else(|| CompileError::new(field, FieldError::MissingDoc))?;

        Ok(Self { ident, ignore, doc })
    }
}

#[derive(Debug)]
struct Struct<'ast> {
    ident: &'ast Ident,
    fields: Vec<StructField<'ast>>,
}

impl<'ast> TryFrom<&'ast DeriveInput> for Struct<'ast> {
    type Error = TokenStream;

    #[rustfmt::skip]
    fn try_from(ast: &'ast DeriveInput) -> Result<Self, Self::Error> {
        let ident = &ast.ident;

        let fields = match &ast.data {
            Data::Struct(data_struct) => &data_struct.fields,
            Data::Enum(data_enum) => {
                return Err(CompileError::new(data_enum.enum_token, InvalidType::Enum));
            }
            Data::Union(data_union) => {
                return Err(CompileError::new(data_union.union_token, InvalidType::Union));
            }
        };

        let punctuated = match fields {
            Fields::Named(fields_named) => &fields_named.named,
            Fields::Unnamed(fields_unnamed) => {
                return Err(CompileError::new(fields_unnamed, InvalidFieldType::Tuple));
            }
            Fields::Unit => {
                return Err(CompileError::new(ident, InvalidFieldType::Unit));
            }
        };

        let fields = punctuated
            .iter()
            .map(StructField::try_from)
            .collect::<Result<Vec<StructField<'_>>, TokenStream>>()?;

        Ok(Self { ident, fields })
    }
}

#[proc_macro_derive(Describe, attributes(ignore_field))]
pub fn describe(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    match Struct::try_from(&ast) {
        Ok(data_struct) => dbg!(data_struct),
        Err(compile_error) => return compile_error,
    };

    let expanded = quote! {}; // TODO

    TokenStream::from(expanded)
}
