use std::ops::{Deref, Index};

use python_ast::{
    self as ast,
    visitor::{self, Visitor},
    AnyNodeRef, AnyParameterRef, Expr, Pattern, Stmt,
};
use ruff_index::{newtype_index, IndexVec};
use serde::{Deserialize, Serialize};

#[newtype_index]
#[derive(Serialize, Deserialize)]
pub struct NodeId;

#[derive(Debug)]
pub struct NodeWithParent<'a> {
    node: AnyNodeRef<'a>,
    parent: Option<NodeId>,
}

impl<'a> NodeWithParent<'a> {
    pub fn parent_id(&self) -> Option<NodeId> {
        self.parent
    }

    pub fn node(&self) -> &AnyNodeRef<'a> {
        &self.node
    }
}

impl<'a> Deref for NodeWithParent<'a> {
    type Target = AnyNodeRef<'a>;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

#[derive(Debug, Default)]
pub struct Nodes<'a>(IndexVec<NodeId, NodeWithParent<'a>>);

impl<'a> Nodes<'a> {
    pub fn insert(&mut self, node: AnyNodeRef<'a>, parent: Option<NodeId>) -> NodeId {
        self.0.push(NodeWithParent { node, parent })
    }

    pub fn parend_id(&self, node_id: NodeId) -> Option<NodeId> {
        self.0[node_id].parent
    }

    pub fn get(&self, node_id: NodeId) -> Option<&NodeWithParent<'a>> {
        self.0.get(node_id)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, NodeWithParent<'_>> {
        self.0.iter()
    }
}

impl<'a> Index<NodeId> for Nodes<'a> {
    type Output = AnyNodeRef<'a>;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.0[index].node
    }
}

#[derive(Default)]
pub struct NodeStack<'a> {
    curr_node: Option<NodeId>,
    nodes: Nodes<'a>,
    is_thirdparty: bool,
}

impl<'a> NodeStack<'a> {
    pub fn build(mut self, ast: &'a [Stmt]) -> Self {
        self.visit_body(ast);
        self
    }

    pub fn is_thirdparty(mut self, value: bool) -> Self {
        self.is_thirdparty = value;
        self
    }

    pub fn get(&self, id: NodeId) -> Option<&NodeWithParent<'a>> {
        self.nodes.get(id)
    }

    pub fn nodes(&self) -> &Nodes {
        &self.nodes
    }

    fn push_node(&mut self, node: impl Into<AnyNodeRef<'a>>) -> NodeId {
        let node_id = self.nodes.insert(node.into(), self.curr_node);
        self.curr_node = Some(node_id);
        node_id
    }

    fn pop_node(&mut self) {
        let node_id = self.curr_node.expect("Attempt to pop without node");
        self.curr_node = self.nodes.parend_id(node_id);
    }
}

impl<'a, 'b> Visitor<'b> for NodeStack<'a>
where
    'b: 'a,
{
    fn visit_stmt(&mut self, stmt: &'b ast::Stmt) {
        self.push_node(stmt);

        match stmt {
            Stmt::FunctionDef(ast::FunctionDefStmt {
                body,
                parameters,
                decorator_list,
                ..
            }) => {
                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }
                self.visit_parameters(parameters);
                // If the current file is from the python stdlib or "site-packages"
                // we should only visit the first statement of the function body.
                if self.is_thirdparty && !body.is_empty() {
                    self.visit_body(&body[..1]);
                } else {
                    self.visit_body(body);
                }
            }
            Stmt::ClassDef(ast::ClassDefStmt {
                body,
                decorator_list,
                ..
            }) => {
                for decorator in decorator_list {
                    self.visit_decorator(decorator);
                }
                self.visit_body(body);
            }
            Stmt::ImportFrom(ast::ImportFromStmt { names, .. }) => {
                for name in names {
                    self.visit_alias(name);
                }
            }
            _ => visitor::walk_stmt(self, stmt),
        }

        self.pop_node();
    }

    fn visit_type_param(&mut self, type_param: &'b ast::TypeParam) {
        self.push_node(type_param);
        match type_param {
            ast::TypeParam::TypeVar(type_var) => {
                if let Some(bound) = type_var.bound.as_ref() {
                    self.visit_expr(bound);
                }
                if let Some(default) = type_var.default.as_ref() {
                    self.visit_expr(default);
                }
            }
            ast::TypeParam::ParamSpec(param_spec) => {
                if let Some(default) = param_spec.default.as_ref() {
                    self.visit_expr(default);
                }
            }

            ast::TypeParam::TypeVarTuple(type_var_tuple) => {
                if let Some(default) = type_var_tuple.default.as_ref() {
                    self.visit_expr(default);
                }
            }
        }
        self.pop_node();
    }

    fn visit_alias(&mut self, alias: &'b ast::Alias) {
        self.push_node(alias);
        self.pop_node();
    }

    fn visit_except_handler(&mut self, except_handler: &'b ast::ExceptHandler) {
        self.push_node(except_handler);
        match except_handler {
            ast::ExceptHandler::ExceptHandler(ast::ExceptHandlerExceptHandler { body, .. }) => {
                self.visit_body(body);
            }
        }
        self.pop_node();
    }

    fn visit_with_item(&mut self, with_item: &'b ast::WithItem) {
        self.push_node(with_item);
        if let Some(optional_vars) = with_item.optional_vars.as_ref() {
            self.visit_expr(optional_vars);
        }
        self.visit_expr(&with_item.context_expr);
        self.pop_node();
    }

    fn visit_parameters(&mut self, parameters: &'b ast::Parameters) {
        self.push_node(parameters);

        for parameter_with_default in parameters.iter_non_variadic_params() {
            self.push_node(parameter_with_default);
            self.visit_parameter(&parameter_with_default.parameter);
            if let Some(default) = &parameter_with_default.default {
                self.visit_expr(default);
            }
            self.pop_node();
        }

        for parameter in parameters.iter().filter_map(AnyParameterRef::as_variadic) {
            self.visit_parameter(parameter);
        }

        self.pop_node();
    }

    fn visit_parameter(&mut self, parameter: &'b ast::Parameter) {
        self.push_node(parameter);
        if let Some(annotation) = &parameter.annotation {
            self.visit_expr(annotation);
        }
        self.pop_node();
    }

    fn visit_pattern(&mut self, pattern: &'b ast::Pattern) {
        self.push_node(pattern);
        match pattern {
            Pattern::MatchSequence(ast::PatternMatchSequence { patterns, .. }) => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
            }
            _ => visitor::walk_pattern(self, pattern),
        }
        self.pop_node();
    }

    fn visit_pattern_keyword(&mut self, pattern_keyword: &'b python_ast::PatternKeyword) {
        self.push_node(pattern_keyword);
        self.visit_pattern(&pattern_keyword.pattern);
        self.pop_node();
    }

    fn visit_comprehension(&mut self, comprehension: &'b ast::Comprehension) {
        self.push_node(comprehension);

        self.visit_expr(&comprehension.iter);
        self.visit_expr(&comprehension.target);
        for expr in &comprehension.ifs {
            self.visit_expr(expr);
        }

        self.pop_node();
    }

    fn visit_expr(&mut self, expr: &'b ast::Expr) {
        self.push_node(expr);
        match expr {
            Expr::Attribute(ast::AttributeExpr { value, .. }) => {
                self.visit_expr(value);
            }
            Expr::Lambda(ast::LambdaExpr {
                body, parameters, ..
            }) => {
                if let Some(parameters) = parameters {
                    self.visit_parameters(parameters);
                }
                self.visit_expr(body);
            }
            _ => visitor::walk_expr(self, expr),
        }
        self.pop_node();
    }
}
