use sith_python_ast::{self as ast, AnyNodeRef};

pub fn get_documentation_string_from_node(node: &AnyNodeRef) -> Option<String> {
    match node {
        AnyNodeRef::StmtClassDef(ast::ClassDefStmt { body, .. })
        | AnyNodeRef::StmtFunctionDef(ast::FunctionDefStmt { body, .. }) => body
            .first()
            .and_then(|stmt| stmt.as_expr_stmt())
            .and_then(|expr_stmt| expr_stmt.value.as_string_literal_expr())
            .map(|str_expr| sanitize_doc_string(str_expr.value.to_str())),
        _ => None,
    }
}

fn sanitize_doc_string(string: &str) -> String {
    let mut result = String::new();
    for line in string.lines() {
        result.push_str(line.trim_start());
        result.push('\n');
    }
    result
}
