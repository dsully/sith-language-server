use python_ast::AnyNodeRef;

pub fn get_documentation_string(node: &AnyNodeRef) -> Option<String> {
    match node {
        AnyNodeRef::StmtClassDef(python_ast::ClassDefStmt { body, .. })
        | AnyNodeRef::StmtFunctionDef(python_ast::FunctionDefStmt { body, .. }) => body
            .first()
            .and_then(|stmt| stmt.as_expr_stmt())
            .and_then(|expr_stmt| expr_stmt.value.as_string_literal_expr())
            .map(|str_expr| str_expr.value.to_string()),
        _ => None,
    }
}
