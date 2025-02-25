pub mod nodes;

use std::borrow::Borrow;

use nodes::{NodeWithParent, Nodes};
use python_ast::{
    visitor::{self, Visitor},
    AnyNodeRef, AttributeExpr, CallExpr, ClassDefStmt, Expr, FunctionDefStmt, NameExpr, Stmt,
};
use ruff_python_resolver::module_descriptor::ImportModuleDescriptor;
use ruff_source_file::LineIndex;
use ruff_text_size::{Ranged, TextRange, TextSize};

pub fn node_at_row<'a>(
    nodes: &'a Nodes,
    offset: u32,
    index: &LineIndex,
) -> Option<&'a NodeWithParent<'a>> {
    let row = index.line_index(offset.into());
    nodes
        .iter()
        .rev()
        .find(|node| index.line_index(node.range().start()) == row)
}

pub fn node_at_offset<'a>(nodes: &'a Nodes, offset: u32) -> Option<&'a NodeWithParent<'a>> {
    nodes
        .iter()
        .rev()
        .find(|node| node.range().contains_inclusive(offset.into()))
}

pub fn identifier_from_node<'a>(node: &'a AnyNodeRef<'a>, offset: u32) -> Option<&'a str> {
    let offset: TextSize = offset.into();
    Some(match node {
        AnyNodeRef::NameExpr(python_ast::NameExpr { id, .. }) => id,
        AnyNodeRef::Parameter(python_ast::Parameter { name, .. }) => name.as_str(),
        AnyNodeRef::StmtFunctionDef(python_ast::FunctionDefStmt { name, .. })
            if name.range.contains_inclusive(offset) =>
        {
            name.as_str()
        }
        AnyNodeRef::StmtClassDef(python_ast::ClassDefStmt { name, .. })
            if name.range.contains_inclusive(offset) =>
        {
            name.as_str()
        }
        AnyNodeRef::Alias(python_ast::Alias { name, .. }) if name.range.contains(offset) => {
            if name.contains('.') {
                identifier_at_offset(name, offset.to_u32(), name.range)?
            } else {
                name.as_str()
            }
        }
        AnyNodeRef::StmtImportFrom(python_ast::ImportFromStmt {
            module: Some(module),
            ..
        }) if module.range.contains(offset) => {
            identifier_at_offset(module, offset.to_u32(), module.range)?
        }
        AnyNodeRef::AttributeExpr(python_ast::AttributeExpr { value, attr, .. }) => {
            if attr.range().contains_inclusive(offset) {
                return Some(attr);
            }

            find_attribute_name_at_offset(value, offset)
        }
        AnyNodeRef::PatternMatchAs(python_ast::PatternMatchAs {
            name: Some(name), ..
        }) if name.range().contains_inclusive(offset) => name.as_str(),
        _ => return None,
    })
}

fn find_attribute_name_at_offset(expr: &Expr, offset: TextSize) -> &str {
    match expr {
        Expr::Attribute(python_ast::AttributeExpr { value, attr, .. }) => {
            if attr.range().contains(offset) {
                attr
            } else {
                find_attribute_name_at_offset(value, offset)
            }
        }
        Expr::Call(python_ast::CallExpr { func, .. }) => {
            find_attribute_name_at_offset(func, offset)
        }
        Expr::Name(python_ast::NameExpr { id, .. }) => id,
        _ => unreachable!(),
    }
}

/// Determines which identifier in a dot-separated string an offset falls into.
fn identifier_at_offset(string: &str, offset: u32, range: TextRange) -> Option<&str> {
    let relative_offset = (offset - range.start().to_u32()) as usize;
    let mut current_start = 0;

    for part in string.split('.') {
        let part_end = current_start + part.len();

        if relative_offset >= current_start && relative_offset < part_end {
            return Some(part);
        }

        // Move to the next part, +1 for the dot
        current_start = part_end + 1;
    }

    None
}

pub fn create_import_module_descriptor<'a, T>(import_stmt: T) -> ImportModuleDescriptor
where
    T: Borrow<AnyNodeRef<'a>>,
{
    match import_stmt.borrow() {
        AnyNodeRef::Alias(alias) => ImportModuleDescriptor {
            leading_dots: 0,
            name_parts: alias
                .name
                .split('.')
                .map(|part| part.trim_end().to_string())
                .filter(|part| !part.is_empty())
                .collect(),
            imported_symbols: vec![],
        },
        AnyNodeRef::StmtImportFrom(import) => ImportModuleDescriptor {
            leading_dots: import.level as usize,
            name_parts: import
                .module
                .as_ref()
                .map(|module| {
                    module
                        .split('.')
                        .map(|part| part.trim_end().to_string())
                        .filter(|part| !part.is_empty())
                        .collect()
                })
                .unwrap_or(Vec::new()),
            imported_symbols: import
                .names
                .iter()
                .map(|alias| alias.name.to_string())
                .collect(),
        },
        _ => panic!("Not an import statement"),
    }
}

#[derive(Default)]
pub struct ReturnStmtCollector<'a> {
    returns: Vec<&'a python_ast::ReturnStmt>,
}

impl<'a> ReturnStmtCollector<'a> {
    pub fn collect(&mut self, body: &'a [Stmt]) -> &[&'a python_ast::ReturnStmt] {
        self.visit_body(body);
        &self.returns
    }
}

impl<'a, 'b> Visitor<'b> for ReturnStmtCollector<'a>
where
    'b: 'a,
{
    fn visit_stmt(&mut self, stmt: &'b Stmt) {
        match stmt {
            Stmt::Return(return_stmt) => self.returns.push(return_stmt),
            _ => visitor::walk_stmt(self, stmt),
        }
    }
}

pub fn is_class_or_function_deprecated(node: &AnyNodeRef) -> bool {
    match node {
        AnyNodeRef::StmtClassDef(ClassDefStmt { decorator_list, .. })
        | AnyNodeRef::StmtFunctionDef(FunctionDefStmt { decorator_list, .. }) => decorator_list
            .iter()
            .any(|decorator| is_deprecated_annotation(&decorator.expression)),
        _ => false,
    }
}

fn is_deprecated_annotation(node: &Expr) -> bool {
    match node {
        Expr::Name(NameExpr { id, .. }) if id == "deprecated" => true,
        Expr::Attribute(AttributeExpr { attr, .. }) if attr == "deprecated" => true,
        Expr::Call(CallExpr { func, .. }) => is_deprecated_annotation(func),
        _ => false,
    }
}
