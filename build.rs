// build.rs

use codegen::Scope;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let out_dir = "src";
    let dest_path = Path::new(&out_dir).join("expr.rs");

    fs::write(&dest_path, define_asts()).unwrap();
    Command::new("cargo")
        .arg("fmt")
        .output()
        .expect("failed to run rustfmt");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/expr.rs");
}

fn define_asts() -> String {
    let mut scope = Scope::new();
    scope.import("crate::scanner", "Token");
    scope.import("crate::scanner", "Literal");
    let expr_enum_name = "Expr";
    let types = [
        (
            "Binary",
            vec!["Box<Expr> left", "Token operator", "Box<Expr> right"],
        ),
        ("Grouping", vec!["Box<Expr>"]),
        ("LiteralNode", vec!["Literal"]),
        ("Unary", vec!["Token operator", "Box<Expr> right"]),
        ("Variable", vec!["Token name"]),
        ("Assign", vec!["Token name", "Box<Expr> value"]),
    ];
    define_ast(expr_enum_name, &types, &mut scope);

    let stmt_enum_name = "Stmt";
    let types = [
        ("Expression", vec![expr_enum_name]),
        ("Print", vec![expr_enum_name]),
        ("Var", vec!["Token name", "Option<Expr> initializer"]),
        ("Block", vec!["Vec<Stmt> statements"]),

    ];
    define_ast(stmt_enum_name, &types, &mut scope);

    scope.to_string()
}

fn define_ast(name: &str, types: &[(&str, Vec<&str>)], scope: &mut Scope) {
    let ast_enum = scope.new_enum(name).vis("pub");
    for (name, fields) in types.iter() {
        let variant = ast_enum.new_variant(name);
        for field in fields {
            let split: Vec<_> = field.split(' ').collect();
            if split.len() == 1 {
                variant.tuple(split[0]);
            } else {
                variant.named(split[1], split[0]);
            }
        }
    }
}
