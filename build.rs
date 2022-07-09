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
    scope.import("crate::interpreter", "Type");
    let expr_enum_name = "Expr";
    let types = [
        (
            "Binary",
            vec!["Box<Expr> left", "Token operator", "Box<Expr> right"],
        ),
        ("Grouping", vec!["Box<Expr>"]),
        ("LiteralNode", vec!["Literal"]),
        ("Unary", vec!["Token operator", "Box<Expr> right"]),
        ("Variable", vec!["Token name", "usize id"]),
        ("Assign", vec!["Token name", "Box<Expr> value", "usize id"]),
        (
            "Logical",
            vec!["Box<Expr> left", "Token operator", "Box<Expr> right"],
        ),
        (
            "Call",
            vec!["Box<Expr> callee", "Token paren", "Vec<Expr> args"],
        ),
        ("Get", vec!["Box<Expr> object", "Token name"]),
        (
            "Set",
            vec!["Box<Expr> object", "Token name", "Box<Expr> value"],
        ),
        ("This", vec!["Token keyword", "usize id"]),
        ("Super", vec!["Token keyword", "Token method", "usize id"]),
    ];
    define_ast(expr_enum_name, &types, &mut scope, &["Clone"]);

    let stmt_enum_name = "Stmt";
    let jump_name = "Jump";
    let types = [
        ("Expression", vec![expr_enum_name]),
        ("Print", vec![expr_enum_name]),
        ("Var", vec!["Token name", "Option<Expr> initializer"]),
        ("Block", vec!["Vec<Stmt> statements"]),
        (
            "If",
            vec![
                "Expr condition",
                "Box<Stmt> then_branch",
                "Option<Box<Stmt>> else_branch",
            ],
        ),
        ("Control", vec![jump_name]),
        ("While", vec!["Expr condition", "Box<Stmt> body"]),
        (
            "Function",
            vec!["Token name", "Vec<Token> params", "Vec<Stmt> body"],
        ),
        (
            "Class",
            vec!["Token name", "Vec<Stmt> methods", "Option<Expr> superclass"],
        ),
    ];
    define_ast(stmt_enum_name, &types, &mut scope, &["Clone"]);
    let types = vec![
        ("Break", vec!["Token keyword"]),
        ("Continue", vec!["Token keyword"]),
        ("ReturnExpr", vec!["Token keyword", "Option<Expr> value"]),
        ("ReturnValue", vec!["Token keyword", "Type value"]),
    ];
    define_ast(jump_name, &types, &mut scope, &["Clone"]);
    scope.to_string()
}

fn define_ast(name: &str, types: &[(&str, Vec<&str>)], scope: &mut Scope, traits: &[&str]) {
    let ast_enum = scope.new_enum(name).vis("pub");
    for trait_d in traits {
        ast_enum.derive(trait_d);
    }
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
