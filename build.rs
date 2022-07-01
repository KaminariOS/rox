// build.rs

use codegen::Scope;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let out_dir = "src";
    let dest_path = Path::new(&out_dir).join("expr.rs");

    fs::write(&dest_path, define_ast()).unwrap();
    Command::new("cargo")
        .arg("fmt")
        .output()
        .expect("failed to run rustfmt");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/expr.rs");
}

fn define_ast() -> String {
    let mut scope = Scope::new();
    scope.import("crate::scanner", "Token");
    scope.import("crate::scanner", "Literal");
    let expr_enum_name = "Expr";
    let expr_enum = scope.new_enum(expr_enum_name).vis("pub");
    let types = [
        (
            "Binary",
            vec!["Box<Expr> left", "Token operator", "Box<Expr> right"],
        ),
        ("Grouping", vec!["Box<Expr>"]),
        ("LiteralNode", vec!["Literal"]),
        ("Unary", vec!["Token operator", "Box<Expr> right"]),
    ];

    for (name, fields) in types.iter() {
        let variant = expr_enum.new_variant(name);
        for field in fields {
            let split: Vec<_> = field.split(' ').collect();
            if split.len() == 1 {
                variant.tuple(split[0]);
            } else {
                variant.named(split[1], split[0]);
            }
        }
    }

    let stmt_enum_name = "Stmt";
    let statement_enum = scope.new_enum(stmt_enum_name).vis("pub");
    let types = [
        ("Expression", vec![expr_enum_name]),
        ("Print", vec![expr_enum_name]),
    ];
    for (name, fields) in types.iter() {
        let variant = statement_enum.new_variant(name);
        for field in fields {
            let split: Vec<_> = field.split(' ').collect();
            if split.len() == 1 {
                variant.tuple(split[0]);
            } else {
                variant.named(split[1], split[0]);
            }
        }
    }

    scope.to_string()
}
