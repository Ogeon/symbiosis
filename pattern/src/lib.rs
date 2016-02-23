extern crate symbiosis_core;
extern crate aster;
extern crate syntex_syntax as syntax;

use self::aster::AstBuilder;
use self::aster::struct_field::StructFieldBuilder;
use self::syntax::ptr::P;
use self::syntax::ast::{Item, Block, ExprKind, CaptureBy};
use std::collections::hash_map::HashMap;

pub use symbiosis_core::pattern::{AnnotatedPattern, AnnotatedComponent, ComponentKind, ParseError};

pub fn build_ast(pattern: &AnnotatedPattern) -> Vec<P<Item>> {
    let builder = AstBuilder::new();
    let mut definitions = HashMap::new();
    register_definitions(pattern, &mut definitions);

    let mut items: Vec<_> = definitions.iter().map(|(&name, fields)| {
        let item = builder.item().pub_();
        if fields.len() == 0 {
            item.unit_struct(name)
        } else {
            item.struct_(name).with_fields(fields.iter().map(|(&field, ty)| {
                let field = StructFieldBuilder::named(field).pub_();
                match *ty {
                    DecodeType::Input => field.ty().path().global().ids(&["symbiosis_tokenizer", "fragment", "InputType"]).build(),
                    DecodeType::String => field.ty().path().global().ids(&["symbiosis_tokenizer", "StrTendril"]).build(),
                    DecodeType::Optional(ty) => field.ty().option().id(ty),
                    DecodeType::Repeat(ty) => field.ty().path().global().ids(&["std", "vec"]).segment("Vec").ty().id(ty).build().build(),
                }
            })).build()
        }
    }).collect();

    let return_ty = if let Some(ref name) = pattern.name {
        builder.ty().id(name)
    } else {
        builder.ty().unit()
    };

    let block = builder.block().stmt().let_().mut_id("args").expr().method_call("into_iter").id("args").build();

    items.push(builder.item().pub_().fn_("decode")
        .arg().id("args")
            .ty().path().global().ids(&["std", "vec"]).segment("Vec")
                .ty().path().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Argument"]).build()
                .build()
            .build()
        .return_()
            .path().global().ids(&["std", "result"]).segment("Result")
                .ty().build(return_ty)
                .ty().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error"]).build()
            .build().build()
        .build(block.expr().build_block(decode(pattern)))
    );
    items.push(builder.item().pub_().fn_("pattern")
        .return_().path().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Pattern"]).build()
        .build(build_pattern(pattern))
    );

    items
}

fn register_definitions<'a>(pattern: &'a AnnotatedPattern, definitions: &mut HashMap<&'a str, HashMap<&'a str, DecodeType<'a>>>) {
    if let Some(ref name) = pattern.name {
        let types = pattern.components.iter().filter_map(|component| {
            if let Some(ref field) = component.name {
                match component.kind {
                    ComponentKind::Input => Some(DecodeType::Input),
                    ComponentKind::String => Some(DecodeType::String),
                    ComponentKind::Optional(ref pattern) => {
                        register_definitions(pattern, definitions);
                        pattern.name.as_ref().map(|t| DecodeType::Optional(t))
                    },
                    ComponentKind::Repeat { ref pattern, .. } => {
                        register_definitions(pattern, definitions);
                        pattern.name.as_ref().map(|t| DecodeType::Repeat(t))
                    },
                    ComponentKind::Token(_) => None,
                }.map(|ty| (&**field, ty))
            } else {
                None
            }
        }).collect();
        definitions.insert(name, types);
    }
}

fn decode(pattern: &AnnotatedPattern) -> P<Block> {
    let builder = AstBuilder::new();
    let mut block = builder.block();

    for component in &pattern.components {
        let (err_fn, arm_expr) = match component.kind {
            ComponentKind::Input => {
                let expr = builder.expr().call().id("try!")
                        .arg().method_call("map_err").method_call("into_input").id("item").build()
                            .arg().build_expr_kind(ExprKind::Closure(
                                CaptureBy::Ref,
                                builder.fn_decl().arg().id("e").ty().infer().return_().infer(),
                                builder.block().expr().call().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error", "expected_input"]).build()
                                    .arg().some().id("e")
                                    .build()
                            )).build()
                        .build();
                ("expected_input", expr)
            },
            ComponentKind::String => {
                let expr = builder.expr().call().id("try!")
                        .arg().method_call("map_err").method_call("into_string").id("item").build()
                            .arg().build_expr_kind(ExprKind::Closure(
                                CaptureBy::Ref,
                                builder.fn_decl().arg().id("e").ty().infer().return_().infer(),
                                builder.block().expr().call().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error", "expected_string"]).build()
                                    .arg().some().id("e")
                                    .build()
                            )).build()
                        .build();
                ("expected_string", expr)
            },
            ComponentKind::Token(_) => {
                let expr = builder.expr().call().id("try!")
                        .arg().method_call("map_err").method_call("into_token").id("item").build()
                            .arg().build_expr_kind(ExprKind::Closure(
                                CaptureBy::Ref,
                                builder.fn_decl().arg().id("e").ty().infer().return_().infer(),
                                builder.block().expr().call().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error", "expected_token"]).build()
                                    .arg().some().id("e")
                                    .build()
                            )).build()
                        .build();
                ("expected_token", expr)
            },
            ComponentKind::Repeat { ref pattern, .. } => {
                let expr = builder.expr().call().id("try!")
                        .arg().method_call("and_then").method_call("map_err").method_call("into_repeat").id("item").build()
                            .arg().build_expr_kind(ExprKind::Closure(
                                CaptureBy::Ref,
                                builder.fn_decl().arg().id("e").ty().infer().return_().infer(),
                                builder.block().expr().call().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error", "expected_repeating"]).build()
                                    .arg().some().id("e")
                                    .build()
                            )).build()
                            .arg().build_expr_kind(ExprKind::Closure(
                                CaptureBy::Ref,
                                builder.fn_decl().arg().id("v").ty().infer().return_().infer(),
                                builder.block()
                                    .stmt().let_().mut_id("res").expr().call().path().global().ids(&["std", "vec", "Vec", "with_capacity"]).build()
                                        .arg().method_call("len").id("v").build()
                                        .build()
                                    .stmt().let_().mut_id("v").expr().method_call("into_iter").id("v").build()
                                    .stmt().expr().loop_().block().expr()
                                        .match_().method_call("next").id("v").build()
                                            .arm().pat().enum_().id("Some").build().pat().id("p").build().body().block()
                                                .stmt().let_().mut_id("args").expr().method_call("into_iter").id("p").build()
                                                .stmt().let_().id("r").ty().path().global().ids(&["std", "result"]).segment("Result")
                                                    .ty().infer()
                                                    .ty().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error"]).build()
                                                    .build().build().expr().build_block(decode(pattern))
                                                .stmt().expr().method_call("push").id("res")
                                                    .arg().call().id("try!").arg().id("r").build()
                                                    .build()
                                                .build()
                                            .arm().pat().id("None").body().break_()
                                            .build()
                                    .expr().ok().id("res")
                            )).build()
                        .build();
                ("expected_repeating", expr)
            },
            ComponentKind::Optional(ref pattern) => {
                let expr = builder.expr().call().id("try!")
                        .arg().method_call("and_then").method_call("map_err").method_call("into_optional").id("item").build()
                            .arg().build_expr_kind(ExprKind::Closure(
                                CaptureBy::Ref,
                                builder.fn_decl().arg().id("e").ty().infer().return_().infer(),
                                builder.block().expr().call().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error", "expected_optional"]).build()
                                    .arg().some().id("e")
                                    .build()
                            )).build()
                            .arg().build_expr_kind(ExprKind::Closure(
                                CaptureBy::Ref,
                                builder.fn_decl().arg().id("p").ty().infer().return_().infer(),
                                builder.block()
                                    .expr().match_().id("p")
                                        .arm().pat().enum_().id("Some").build().pat().id("p").build().body().block()
                                            .stmt().let_().mut_id("args").expr().method_call("into_iter").id("p").build()
                                            .stmt().let_().id("res").ty().path().global().ids(&["std", "result"]).segment("Result")
                                                .ty().infer()
                                                .ty().path().global().ids(&["symbiosis_tokenizer", "fragment", "Error"]).build()
                                                .build().build().expr().build_block(decode(pattern))
                                            .expr().ok().some().call().id("try!").arg().id("res").build()
                                        .arm().pat().id("None").body().ok().none()
                                        .build()
                            )).build()
                        .build();
                ("expected_optional", expr)
            },
        };
        if let Some(ref field) = component.name {
            block = block.stmt().let_id(field).match_().method_call("next").id("args").build()
                .arm().pat().enum_().id("Some").build().pat().id("item").build().body().build(arm_expr)
                .arm().pat().id("None").body().return_expr().err().call()
                    .path().global().ids(&["symbiosis_tokenizer", "fragment", "Error", err_fn]).build()
                    .arg().none()
                    .build()
                .build();
        } else {
            block = block.stmt().expr().match_().method_call("next").id("args").build()
                .arm().pat().enum_().id("Some").build().pat().id("item").build().body().block().stmt().expr().build(arm_expr).build()
                .arm().pat().id("None").body().return_expr().err().call()
                    .path().global().ids(&["symbiosis_tokenizer", "fragment", "Error", err_fn]).build()
                    .arg().none()
                    .build()
                .build();
        }
    }

    if let Some(ref name) = pattern.name {
        block.expr().ok().struct_().id(name).build().with_id_exprs(
            pattern.components.iter().filter_map(|component| component.name.as_ref().map(|field|
                (builder.id(field), builder.expr().id(field))
            ))
        ).build()
    } else {
        block.expr().ok().unit()
    }
}

fn build_pattern(pattern: &AnnotatedPattern) -> P<Block> {
    let builder = AstBuilder::new();
    let mut block = builder.block().stmt().let_().mut_id("pattern").expr().call()
        .path().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Pattern", "new"]).build()
        .build();

    for component in &pattern.components {
        let expr = match component.kind {
            ComponentKind::Input => builder.expr().path().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Component", "Input"]).build(),
            ComponentKind::String => builder.expr().path().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Component", "String"]).build(),
            ComponentKind::Token(ref t) => builder.expr().call().path().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Component", "Token"]).build()
                .arg().method_call("into").lit().str(&**t).build()
                .build(),
            ComponentKind::Optional(ref p) => builder.expr().block()
                .stmt().let_id("inner").build_block(build_pattern(p))
                .expr().call().path().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Component", "Optional"]).build()
                    .arg().id("inner")
                    .build(),
            ComponentKind::Repeat { ref pattern, at_least, ref delimiter } => builder.expr().block()
                .stmt().let_id("inner").build_block(build_pattern(pattern))
                .expr().struct_().global().ids(&["symbiosis_tokenizer", "fragment", "pattern", "Component", "Repeat"]).build()
                    .field("pattern").id("inner")
                    .field("at_least").lit().usize(at_least)
                    .field("delimiter").method_call("into").lit().str(&**delimiter).build()
                    .build()
        };

        block = block.stmt().expr().method_call("push").id("pattern")
            .arg().build(expr)
            .build();
    }

    block.expr().id("pattern")
}

enum DecodeType<'a> {
    Input,
    String,
    Optional(&'a str),
    Repeat(&'a str),
}
