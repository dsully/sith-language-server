pub mod nodes;

use std::borrow::Borrow;

use compact_str::CompactString;
use nodes::{NodeWithParent, Nodes};
use ruff_python_resolver::module_descriptor::ImportModuleDescriptor;
use ruff_source_file::LineIndex;
use ruff_text_size::{Ranged, TextRange, TextSize};
use sith_python_ast::{
    self as ast,
    str_prefix::FStringPrefix,
    visitor::{self, Visitor},
    AnyNodeRef, ArgOrKeyword, AttributeExpr, BoolOp, CallExpr, ClassDefStmt, CmpOp, Comprehension,
    Expr, FStringElement, FStringElements, FStringExpr, FStringPart, FunctionDefStmt, NameExpr,
    Number, Operator, ParameterWithDefault, Parameters, Stmt, StringFlags, UnaryOp,
};

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

pub fn node_identifier_at_offset<'a>(node: &'a AnyNodeRef<'a>, offset: u32) -> Option<&'a str> {
    let offset: TextSize = offset.into();
    Some(match node {
        AnyNodeRef::NameExpr(ast::NameExpr { id, .. }) => id,
        AnyNodeRef::Parameter(ast::Parameter { name, .. }) => name.as_str(),
        AnyNodeRef::StmtFunctionDef(ast::FunctionDefStmt { name, .. })
            if name.range.contains_inclusive(offset) =>
        {
            name.as_str()
        }
        AnyNodeRef::StmtClassDef(ast::ClassDefStmt { name, .. })
            if name.range.contains_inclusive(offset) =>
        {
            name.as_str()
        }
        AnyNodeRef::Alias(ast::Alias { name, .. }) if name.range.contains(offset) => {
            if name.contains('.') {
                identifier_at_offset(name, offset.to_u32(), name.range)?
            } else {
                name.as_str()
            }
        }
        AnyNodeRef::StmtImportFrom(ast::ImportFromStmt {
            module: Some(module),
            ..
        }) if module.range.contains(offset) => {
            identifier_at_offset(module, offset.to_u32(), module.range)?
        }
        AnyNodeRef::AttributeExpr(ast::AttributeExpr { value, attr, .. }) => {
            if attr.range().contains_inclusive(offset) {
                return Some(attr);
            }

            find_attribute_name_at_offset(value, offset)
        }
        AnyNodeRef::PatternMatchAs(ast::PatternMatchAs {
            name: Some(name), ..
        }) if name.range().contains_inclusive(offset) => name.as_str(),
        _ => return None,
    })
}

pub fn call_expr_identifier(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Name(ast::NameExpr { id, .. }) => Some(id.as_str()),
        Expr::Call(ast::CallExpr { func, .. }) => call_expr_identifier(func),
        Expr::Attribute(ast::AttributeExpr { attr, .. }) => Some(attr.as_str()),
        _ => None,
    }
}

fn find_attribute_name_at_offset(expr: &Expr, offset: TextSize) -> &str {
    match expr {
        Expr::Attribute(ast::AttributeExpr { value, attr, .. }) => {
            if attr.range().contains(offset) {
                attr
            } else {
                find_attribute_name_at_offset(value, offset)
            }
        }
        Expr::Call(ast::CallExpr { func, .. }) => find_attribute_name_at_offset(func, offset),
        Expr::Name(ast::NameExpr { id, .. }) => id,
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
    returns: Vec<&'a ast::ReturnStmt>,
}

impl<'a> ReturnStmtCollector<'a> {
    pub fn collect(&mut self, body: &'a [Stmt]) -> &[&'a ast::ReturnStmt] {
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

pub fn is_function_overloaded(node: &AnyNodeRef) -> bool {
    match node {
        AnyNodeRef::StmtFunctionDef(FunctionDefStmt { decorator_list, .. }) => decorator_list
            .iter()
            .any(|decorator| is_overload_annotation(&decorator.expression)),
        _ => false,
    }
}

fn is_overload_annotation(node: &Expr) -> bool {
    match node {
        Expr::Name(NameExpr { id, .. }) if id == "overload" => true,
        Expr::Attribute(AttributeExpr { attr, .. }) if attr == "overload" => true,
        Expr::Call(CallExpr { func, .. }) => is_deprecated_annotation(func),
        _ => false,
    }
}

pub fn expr_to_str(expr: &Expr) -> String {
    let mut result = String::new();
    match expr {
        Expr::BoolOp(bool_op_expr) => {
            let last_index = bool_op_expr.values.len() - 1;
            for (i, value) in bool_op_expr.values.iter().enumerate() {
                result.push_str(&expr_to_str(value));
                if i != last_index {
                    match bool_op_expr.op {
                        BoolOp::And => result.push_str(" and "),
                        BoolOp::Or => result.push_str(" or "),
                    }
                }
            }
        }
        Expr::Named(named_expr) => {
            result.push_str(&format!(
                "({} := {})",
                expr_to_str(&named_expr.target),
                expr_to_str(&named_expr.value)
            ));
        }
        // TODO: print parenthesized binary expressions
        Expr::BinOp(bin_op_expr) => {
            let op_str = match bin_op_expr.op {
                Operator::Add => "+",
                Operator::Sub => "-",
                Operator::Mult => "*",
                Operator::MatMult => "@",
                Operator::Div => "/",
                Operator::Mod => "%",
                Operator::Pow => "**",
                Operator::LShift => "<<",
                Operator::RShift => ">>",
                Operator::BitOr => "|",
                Operator::BitXor => "^",
                Operator::BitAnd => "&",
                Operator::FloorDiv => "//",
            };

            result.push_str(&format!(
                "{} {op_str} {}",
                expr_to_str(&bin_op_expr.left),
                expr_to_str(&bin_op_expr.right)
            ));
        }
        Expr::UnaryOp(unary_op_expr) => {
            let op_str = match unary_op_expr.op {
                UnaryOp::Invert => "~",
                UnaryOp::Not => "not ",
                UnaryOp::UAdd => "+",
                UnaryOp::USub => "-",
            };
            result.push_str(&format!("{op_str}{}", expr_to_str(&unary_op_expr.operand)));
        }
        Expr::Lambda(lambda_expr) => {
            result.push_str("lambda");
            if let Some(parameters) = &lambda_expr.parameters {
                result.push(' ');
                result.push_str(&parameters_to_str(parameters));
            }
            result.push_str(": ");
            result.push_str(&expr_to_str(&lambda_expr.body));
        }
        Expr::If(if_expr) => {
            result.push_str(&format!(
                "{} if {} else {}",
                expr_to_str(&if_expr.body),
                expr_to_str(&if_expr.test),
                expr_to_str(&if_expr.orelse)
            ));
        }
        Expr::Dict(dict_expr) => {
            result.push('{');
            for (i, dict_item) in dict_expr.items.iter().enumerate() {
                if let Some(key) = dict_item.key.as_ref() {
                    result.push_str(&expr_to_str(key));
                    result.push_str(": ");
                } else {
                    result.push_str("**");
                }
                result.push_str(&expr_to_str(&dict_item.value));

                if i < dict_expr.items.len() - 1 {
                    result.push_str(", ");
                }
            }
            result.push('}');
        }
        Expr::Set(set_expr) => {
            result.push('{');
            for (i, elt) in set_expr.elts.iter().enumerate() {
                result.push_str(&expr_to_str(elt));
                if i < set_expr.elts.len() - 1 {
                    result.push_str(", ");
                }
            }
            result.push('}');
        }
        Expr::List(list_expr) => {
            result.push('[');
            for (i, elt) in list_expr.elts.iter().enumerate() {
                result.push_str(&expr_to_str(elt));
                if i < list_expr.elts.len() - 1 {
                    result.push_str(", ");
                }
            }
            result.push(']');
        }
        Expr::Tuple(tuple_expr) => {
            if tuple_expr.parenthesized {
                result.push('(');
            }
            for (i, elt) in tuple_expr.elts.iter().enumerate() {
                result.push_str(&expr_to_str(elt));
                if i < tuple_expr.elts.len() - 1 {
                    result.push_str(", ");
                }
            }
            if tuple_expr.parenthesized {
                result.push(')');
            }
        }
        Expr::ListComp(list_comp_expr) => {
            result.push('[');
            result.push_str(&expr_to_str(&list_comp_expr.elt));
            for comp in &list_comp_expr.generators {
                result.push_str(&comprehension_to_str(comp));
            }
            result.push(']');
        }
        Expr::SetComp(set_comp_expr) => {
            result.push('{');
            result.push_str(&expr_to_str(&set_comp_expr.elt));
            for comp in &set_comp_expr.generators {
                result.push_str(&comprehension_to_str(comp));
            }
            result.push('}');
        }
        Expr::DictComp(dict_comp_expr) => {
            result.push('{');
            result.push_str(&expr_to_str(&dict_comp_expr.key));
            result.push_str(": ");
            result.push_str(&expr_to_str(&dict_comp_expr.value));
            for comp in &dict_comp_expr.generators {
                result.push_str(&comprehension_to_str(comp));
            }
            result.push('}');
        }
        Expr::Generator(generator_expr) => {
            if generator_expr.parenthesized {
                result.push('(');
            }
            result.push_str(&expr_to_str(&generator_expr.elt));
            for comp in &generator_expr.generators {
                result.push_str(&comprehension_to_str(comp));
            }
            if generator_expr.parenthesized {
                result.push(')');
            }
        }
        Expr::Await(await_expr) => {
            result.push_str("await ");
            result.push_str(&expr_to_str(&await_expr.value));
        }
        Expr::Yield(yield_expr) => {
            result.push_str("yield");
            if let Some(value) = &yield_expr.value {
                result.push(' ');
                result.push_str(&expr_to_str(value));
            }
        }
        Expr::YieldFrom(yield_from_expr) => {
            result.push_str("yield from ");
            result.push_str(&expr_to_str(&yield_from_expr.value));
        }
        Expr::Compare(compare_expr) => {
            result.push_str(&expr_to_str(&compare_expr.left));
            for (op, comparator) in compare_expr.ops.iter().zip(&compare_expr.comparators) {
                let op_str = match op {
                    CmpOp::Eq => "==",
                    CmpOp::NotEq => "!=",
                    CmpOp::Lt => "<",
                    CmpOp::LtE => "<=",
                    CmpOp::Gt => ">",
                    CmpOp::GtE => ">=",
                    CmpOp::Is => "is",
                    CmpOp::IsNot => "is not",
                    CmpOp::In => "in",
                    CmpOp::NotIn => "not in",
                };
                result.push(' ');
                result.push_str(op_str);
                result.push(' ');
                result.push_str(&expr_to_str(comparator));
            }
        }
        Expr::Call(call_expr) => {
            result.push_str(&expr_to_str(&call_expr.func));

            let args_str: Vec<String> = call_expr
                .arguments
                .arguments_source_order()
                .map(|arg| match arg {
                    ArgOrKeyword::Arg(expr) => expr_to_str(expr),
                    ArgOrKeyword::Keyword(keyword) => {
                        if let Some(arg_name) = &keyword.arg {
                            format!("{}={}", arg_name.id, expr_to_str(&keyword.value))
                        } else {
                            format!("**{}", expr_to_str(&keyword.value))
                        }
                    }
                })
                .collect();
            result.push_str(&format!("({})", args_str.join(", ")));
        }
        Expr::FString(fstring_expr) => {
            result.push_str(&fstring_expr_to_str(fstring_expr));
        }
        Expr::StringLiteral(string_literal_expr) => {
            let quotes = if string_literal_expr.value.is_triple_quoted() {
                "\"\"\""
            } else {
                "\""
            };
            result.push_str(&format!(
                "{}{quotes}{}{quotes}",
                string_literal_expr.value.prefix(),
                string_literal_expr.value.to_str()
            ));
        }
        Expr::BytesLiteral(bytes_literal_expr) => {
            let quotes = if bytes_literal_expr.value.is_triple_quoted() {
                "\"\"\""
            } else {
                "\""
            };
            result.push_str(&format!(
                "{}{quotes}",
                bytes_literal_expr.value.prefix().as_str()
            ));
            for byte_literal in bytes_literal_expr.value.bytes() {
                result.push(byte_literal as char);
            }
            result.push_str(quotes);
        }
        Expr::NumberLiteral(number_literal_expr) => match &number_literal_expr.value {
            Number::Int(int) => result.push_str(&int.to_string()),
            Number::Float(float) => result.push_str(&float.to_string()),
            Number::Complex { imag, .. } => {
                result.push_str(&imag.to_string());
                result.push('j');
            }
        },
        Expr::BooleanLiteral(boolean_literal_expr) => {
            if boolean_literal_expr.value {
                result.push_str("True");
            } else {
                result.push_str("False");
            }
        }
        Expr::NoneLiteral(_) => result.push_str("None"),
        Expr::EllipsisLiteral(_) => result.push_str("..."),
        Expr::Attribute(attribute_expr) => {
            result.push_str(&expr_to_str(&attribute_expr.value));
            result.push('.');
            result.push_str(&attribute_expr.attr);
        }
        Expr::Subscript(subscript_expr) => {
            result.push_str(&expr_to_str(&subscript_expr.value));
            result.push('[');
            result.push_str(&expr_to_str(&subscript_expr.slice));
            result.push(']');
        }
        Expr::Starred(starred_expr) => {
            result.push('*');
            result.push_str(&expr_to_str(&starred_expr.value));
        }
        Expr::Name(name_expr) => result.push_str(&name_expr.id),
        Expr::Slice(slice_expr) => {
            let lower = slice_expr
                .lower
                .as_ref()
                .map_or(String::new(), |lower| expr_to_str(lower));
            let upper = slice_expr
                .upper
                .as_ref()
                .map_or(String::new(), |upper| expr_to_str(upper));
            let step = slice_expr
                .step
                .as_ref()
                .map_or(String::new(), |step| expr_to_str(step));

            let slice_str = if slice_expr.step.is_some() {
                format!("{}:{}:{}", lower, upper, step)
            } else if slice_expr.lower.is_none()
                && slice_expr.upper.is_none()
                && slice_expr.step.is_none()
                && slice_expr.range.len().to_u32() == 2
            {
                "::".to_string()
            } else {
                format!("{}:{}", lower, upper)
            };
            result.push_str(&slice_str);
        }
        Expr::IpyEscapeCommand(ipy_escape_command_expr) => {
            result.push_str(&format!(
                "{}{}",
                ipy_escape_command_expr.kind, ipy_escape_command_expr.value
            ));
        }
    }

    result
}

fn parameters_to_str(parameters: &Parameters) -> String {
    let mut parts: Vec<CompactString> = Vec::new();

    // 1. Positional-only parameters
    for arg in &parameters.posonlyargs {
        parts.push(parameter_with_default_to_str(arg));
    }
    if !parameters.posonlyargs.is_empty() {
        parts.push(CompactString::new("/"));
    }

    // 2. Regular parameters
    for arg in &parameters.args {
        parts.push(parameter_with_default_to_str(arg));
    }

    // 3. Variable positional argument or lone asterisk
    if let Some(vararg) = &parameters.vararg {
        parts.push(CompactString::new(format!("*{}", vararg.name)));
    } else if !parameters.kwonlyargs.is_empty() {
        parts.push(CompactString::new("*"));
    }

    // 4. Keyword-only parameters
    for arg in &parameters.kwonlyargs {
        parts.push(parameter_with_default_to_str(arg));
    }

    // 5. Variable keyword argument
    if let Some(kwarg) = &parameters.kwarg {
        parts.push(CompactString::new(format!("**{}", kwarg.name)));
    }

    parts.join(", ")
}

pub fn parameter_with_default_to_str(param: &ParameterWithDefault) -> CompactString {
    let mut result = CompactString::default();
    result.push_str(&param.parameter.name);
    if let Some(default) = &param.default {
        result.push_str(" = ");
        result.push_str(&expr_to_str(default));
    }
    result
}

fn comprehension_to_str(comp: &Comprehension) -> String {
    let mut result = String::new();
    if comp.is_async {
        result.push_str("async");
    }
    result.push_str(" for ");
    result.push_str(&expr_to_str(&comp.target));
    result.push_str(" in ");
    result.push_str(&expr_to_str(&comp.iter));

    for if_comp in &comp.ifs {
        result.push_str(" if ");
        result.push_str(&expr_to_str(if_comp));
    }
    result
}

pub fn fstring_expr_to_str(fstring_expr: &FStringExpr) -> String {
    let mut result = String::new();
    for fstring_part in fstring_expr.value.iter() {
        let fstring_part_str = match fstring_part {
            FStringPart::Literal(lit) => format!("\"{}\"", lit.value),
            FStringPart::FString(fstr) => {
                let prefix = match fstr.flags.prefix() {
                    FStringPrefix::Regular => "f",
                    FStringPrefix::Raw { uppercase_r } => {
                        if uppercase_r {
                            "Rf"
                        } else {
                            "rf"
                        }
                    }
                };
                let quote = if fstr.flags.is_triple_quoted() {
                    "\"\"\""
                } else {
                    "\""
                };
                let content = fstring_elements_to_str(&fstr.elements);
                format!("{}{}{}{}", prefix, quote, content, quote)
            }
        };
        result.push_str(&fstring_part_str);
    }

    result
}

// Helper function to handle nested FStringElements in format specs
fn fstring_elements_to_str(elements: &FStringElements) -> String {
    elements
        .iter()
        .map(|element| match element {
            FStringElement::Literal(lit) => lit.value.to_string(),
            FStringElement::Expression(expr) => {
                let expr_str = expr_to_str(&expr.expression);
                let conversion = expr
                    .conversion
                    .to_char()
                    .map(|c| format!("!{}", c))
                    .unwrap_or_default();
                let format_spec = expr
                    .format_spec
                    .as_ref()
                    .map(|spec| format!(":{}", fstring_elements_to_str(&spec.elements)))
                    .unwrap_or_default();
                format!("{{{expr_str}{conversion}{format_spec}}}")
            }
        })
        .collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use sith_python_ast::Expr;
    use sith_python_parser::{parse_unchecked, Mode};

    fn parse_expr(source: &str) -> Expr {
        *parse_unchecked(source, Mode::Expression)
            .into_syntax()
            .expression()
            .unwrap()
            .body
    }

    #[test]
    fn test_bool_op_expr_to_str() {
        let expr = parse_expr("True and False");
        assert_eq!(expr_to_str(&expr), "True and False");

        let expr = parse_expr("x or y or z");
        assert_eq!(expr_to_str(&expr), "x or y or z");
    }

    #[test]
    fn test_named_expr_to_str() {
        let expr = parse_expr("(x := 42)");
        assert_eq!(expr_to_str(&expr), "(x := 42)");
    }

    #[test]
    fn test_bin_op_expr_to_str() {
        let expr = parse_expr("1 + 1");
        assert_eq!(expr_to_str(&expr), "1 + 1");

        let expr = parse_expr("2 * 3");
        assert_eq!(expr_to_str(&expr), "2 * 3");

        let expr = parse_expr("5 - 2");
        assert_eq!(expr_to_str(&expr), "5 - 2");

        let expr = parse_expr("10 / 2");
        assert_eq!(expr_to_str(&expr), "10 / 2");

        let expr = parse_expr("10 // 3");
        assert_eq!(expr_to_str(&expr), "10 // 3");

        let expr = parse_expr("10 % 3");
        assert_eq!(expr_to_str(&expr), "10 % 3");

        let expr = parse_expr("2 ** 3");
        assert_eq!(expr_to_str(&expr), "2 ** 3");

        let expr = parse_expr("8 >> 2");
        assert_eq!(expr_to_str(&expr), "8 >> 2");

        let expr = parse_expr("1 << 3");
        assert_eq!(expr_to_str(&expr), "1 << 3");

        let expr = parse_expr("5 | 3");
        assert_eq!(expr_to_str(&expr), "5 | 3");

        let expr = parse_expr("5 ^ 3");
        assert_eq!(expr_to_str(&expr), "5 ^ 3");

        let expr = parse_expr("5 & 3");
        assert_eq!(expr_to_str(&expr), "5 & 3");

        let expr = parse_expr("a @ b");
        assert_eq!(expr_to_str(&expr), "a @ b");
    }

    #[test]
    fn test_unary_op_expr_to_str() {
        let expr = parse_expr("-x");
        assert_eq!(expr_to_str(&expr), "-x");

        let expr = parse_expr("+y");
        assert_eq!(expr_to_str(&expr), "+y");

        let expr = parse_expr("~z");
        assert_eq!(expr_to_str(&expr), "~z");

        let expr = parse_expr("not condition");
        assert_eq!(expr_to_str(&expr), "not condition");
    }

    #[test]
    fn test_lambda_expr_to_str() {
        let expr = parse_expr("lambda: 42");
        assert_eq!(expr_to_str(&expr), "lambda: 42");

        let expr = parse_expr("lambda x: x + 1");
        assert_eq!(expr_to_str(&expr), "lambda x: x + 1");

        let expr = parse_expr("lambda x, y=10: x * y");
        assert_eq!(expr_to_str(&expr), "lambda x, y=10: x * y");
    }

    #[test]
    fn test_if_expr_to_str() {
        let expr = parse_expr("x if condition else y");
        assert_eq!(expr_to_str(&expr), "x if condition else y");
    }

    #[test]
    fn test_dict_expr_to_str() {
        let expr = parse_expr("{'a': 1, 'b': 2}");
        assert_eq!(expr_to_str(&expr), "{\"a\": 1, \"b\": 2}");

        let expr = parse_expr("{**x, 'c': 3}");
        assert_eq!(expr_to_str(&expr), "{**x, \"c\": 3}");
    }

    #[test]
    fn test_set_expr_to_str() {
        let expr = parse_expr("{1, 2, 3}");
        assert_eq!(expr_to_str(&expr), "{1, 2, 3}");
    }

    #[test]
    fn test_list_expr_to_str() {
        let expr = parse_expr("[1, 2, 3]");
        assert_eq!(expr_to_str(&expr), "[1, 2, 3]");
    }

    #[test]
    fn test_tuple_expr_to_str() {
        let expr = parse_expr("(1, 2, 3)");
        assert_eq!(expr_to_str(&expr), "(1, 2, 3)");
    }

    #[test]
    fn test_list_comp_expr_to_str() {
        let expr = parse_expr("[x for x in range(10)]");
        assert_eq!(expr_to_str(&expr), "[x for x in range(10)]");

        let expr = parse_expr("[x for x in range(10) if x % 2 == 0]");
        assert_eq!(expr_to_str(&expr), "[x for x in range(10) if x % 2 == 0]");
    }

    #[test]
    fn test_set_comp_expr_to_str() {
        let expr = parse_expr("{x for x in range(10)}");
        assert_eq!(expr_to_str(&expr), "{x for x in range(10)}");

        let expr = parse_expr("{x for x in range(10) if x % 2 == 0}");
        assert_eq!(expr_to_str(&expr), "{x for x in range(10) if x % 2 == 0}");
    }

    #[test]
    fn test_dict_comp_expr_to_str() {
        let expr = parse_expr("{x: x**2 for x in range(10)}");
        assert_eq!(expr_to_str(&expr), "{x: x ** 2 for x in range(10)}");

        let expr = parse_expr("{x: x**2 for x in range(10) if x % 2 == 0}");
        assert_eq!(
            expr_to_str(&expr),
            "{x: x ** 2 for x in range(10) if x % 2 == 0}"
        );
    }

    #[test]
    fn test_generator_expr_to_str() {
        let expr = parse_expr("(x for x in range(10))");
        assert_eq!(expr_to_str(&expr), "(x for x in range(10))");

        let expr = parse_expr("(x for x in range(10) if x % 2 == 0)");
        assert_eq!(expr_to_str(&expr), "(x for x in range(10) if x % 2 == 0)");
    }

    #[test]
    fn test_await_expr_to_str() {
        let expr = parse_expr("await future");
        assert_eq!(expr_to_str(&expr), "await future");
    }

    #[test]
    fn test_yield_expr_to_str() {
        let expr = parse_expr("yield");
        assert_eq!(expr_to_str(&expr), "yield");

        let expr = parse_expr("yield x");
        assert_eq!(expr_to_str(&expr), "yield x");
    }

    #[test]
    fn test_yield_from_expr_to_str() {
        let expr = parse_expr("yield from generator");
        assert_eq!(expr_to_str(&expr), "yield from generator");
    }

    #[test]
    fn test_compare_expr_to_str() {
        let expr = parse_expr("x == y");
        assert_eq!(expr_to_str(&expr), "x == y");

        let expr = parse_expr("x != y");
        assert_eq!(expr_to_str(&expr), "x != y");

        let expr = parse_expr("x < y");
        assert_eq!(expr_to_str(&expr), "x < y");

        let expr = parse_expr("x <= y");
        assert_eq!(expr_to_str(&expr), "x <= y");

        let expr = parse_expr("x > y");
        assert_eq!(expr_to_str(&expr), "x > y");

        let expr = parse_expr("x >= y");
        assert_eq!(expr_to_str(&expr), "x >= y");

        let expr = parse_expr("x is y");
        assert_eq!(expr_to_str(&expr), "x is y");

        let expr = parse_expr("x is not y");
        assert_eq!(expr_to_str(&expr), "x is not y");

        let expr = parse_expr("x in y");
        assert_eq!(expr_to_str(&expr), "x in y");

        let expr = parse_expr("x not in y");
        assert_eq!(expr_to_str(&expr), "x not in y");

        // Chain comparison
        let expr = parse_expr("a < b < c");
        assert_eq!(expr_to_str(&expr), "a < b < c");
    }

    #[test]
    fn test_call_expr_to_str() {
        let expr = parse_expr("func()");
        assert_eq!(expr_to_str(&expr), "func()");

        let expr = parse_expr("func(1, 2, 3)");
        assert_eq!(expr_to_str(&expr), "func(1, 2, 3)");

        let expr = parse_expr("func(a=1, b=2)");
        assert_eq!(expr_to_str(&expr), "func(a=1, b=2)");

        let expr = parse_expr("func(*args, **kwargs)");
        assert_eq!(expr_to_str(&expr), "func(*args, **kwargs)");
    }

    #[test]
    fn test_fstring_expr_to_str() {
        let expr = parse_expr("f\"Hello, {name}!\"");
        assert_eq!(expr_to_str(&expr), "f\"Hello, {name}!\"");

        let expr = parse_expr("f\"Value: {value:.2f}\"");
        assert_eq!(expr_to_str(&expr), "f\"Value: {value:.2f}\"");

        let expr = parse_expr("f\"Nested: {f\"Inner: {x}\"}\"");
        assert_eq!(expr_to_str(&expr), "f\"Nested: {f\"Inner: {x}\"}\"");

        let expr = parse_expr("rf\"Raw: {value}\"");
        assert_eq!(expr_to_str(&expr), "rf\"Raw: {value}\"");
    }

    #[test]
    fn test_string_literal_expr_to_str() {
        let expr = parse_expr("\"Hello, world!\"");
        assert_eq!(expr_to_str(&expr), "\"Hello, world!\"");

        let expr = parse_expr("r\"Raw string\"");
        assert_eq!(expr_to_str(&expr), "r\"Raw string\"");

        let expr = parse_expr("\"\"\"Triple quoted string\"\"\"");
        assert_eq!(expr_to_str(&expr), "\"\"\"Triple quoted string\"\"\"");
    }

    #[test]
    fn test_bytes_literal_expr_to_str() {
        let expr = parse_expr("b\"Bytes\"");
        assert_eq!(expr_to_str(&expr), "b\"Bytes\"");

        let expr = parse_expr("rb\"Raw bytes\"");
        assert_eq!(expr_to_str(&expr), "rb\"Raw bytes\"");
    }

    #[test]
    fn test_number_literal_expr_to_str() {
        let expr = parse_expr("42");
        assert_eq!(expr_to_str(&expr), "42");

        let expr = parse_expr("3.14");
        assert_eq!(expr_to_str(&expr), "3.14");

        let expr = parse_expr("1j");
        assert_eq!(expr_to_str(&expr), "1j");
    }

    #[test]
    fn test_boolean_literal_expr_to_str() {
        let expr = parse_expr("True");
        assert_eq!(expr_to_str(&expr), "True");

        let expr = parse_expr("False");
        assert_eq!(expr_to_str(&expr), "False");
    }

    #[test]
    fn test_none_literal_expr_to_str() {
        let expr = parse_expr("None");
        assert_eq!(expr_to_str(&expr), "None");
    }

    #[test]
    fn test_ellipsis_literal_expr_to_str() {
        let expr = parse_expr("...");
        assert_eq!(expr_to_str(&expr), "...");
    }

    #[test]
    fn test_attribute_expr_to_str() {
        let expr = parse_expr("obj.attr");
        assert_eq!(expr_to_str(&expr), "obj.attr");

        let expr = parse_expr("obj.attr.nested");
        assert_eq!(expr_to_str(&expr), "obj.attr.nested");
    }

    #[test]
    fn test_subscript_expr_to_str() {
        let expr = parse_expr("arr[0]");
        assert_eq!(expr_to_str(&expr), "arr[0]");

        let expr = parse_expr("arr[1:10]");
        assert_eq!(expr_to_str(&expr), "arr[1:10]");

        let expr = parse_expr("arr[1:10:2]");
        assert_eq!(expr_to_str(&expr), "arr[1:10:2]");

        let expr = parse_expr("arr[:10]");
        assert_eq!(expr_to_str(&expr), "arr[:10]");

        let expr = parse_expr("arr[1:]");
        assert_eq!(expr_to_str(&expr), "arr[1:]");

        let expr = parse_expr("arr[:]");
        assert_eq!(expr_to_str(&expr), "arr[:]");

        let expr = parse_expr("arr[::]");
        assert_eq!(expr_to_str(&expr), "arr[::]");

        let expr = parse_expr("arr[::2]");
        assert_eq!(expr_to_str(&expr), "arr[::2]");
    }

    #[test]
    fn test_starred_expr_to_str() {
        let expr = parse_expr("*args");
        assert_eq!(expr_to_str(&expr), "*args");
    }

    #[test]
    fn test_name_expr_to_str() {
        let expr = parse_expr("variable_name");
        assert_eq!(expr_to_str(&expr), "variable_name");
    }

    #[test]
    fn test_complex_expressions_to_str() {
        let expr = parse_expr("x + y * z");
        assert_eq!(expr_to_str(&expr), "x + y * z");

        // TODO: handle the parenthesis
        let expr = parse_expr("(x + y) * z");
        assert_eq!(expr_to_str(&expr), "x + y * z");

        let expr = parse_expr("func(a, b=c, *args, **kwargs)");
        assert_eq!(expr_to_str(&expr), "func(a, b=c, *args, **kwargs)");

        let expr = parse_expr("x.y.z[0].method()");
        assert_eq!(expr_to_str(&expr), "x.y.z[0].method()");

        let expr = parse_expr("f\"{x + y} {z:.2f}\"");
        assert_eq!(expr_to_str(&expr), "f\"{x + y} {z:.2f}\"");

        let expr = parse_expr("[x + y for x, y in zip(a, b) if x > 0]");
        assert_eq!(expr_to_str(&expr), "[x + y for x, y in zip(a, b) if x > 0]");
    }
}
