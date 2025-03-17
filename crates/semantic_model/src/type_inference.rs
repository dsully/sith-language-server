use std::{path::PathBuf, sync::Arc};

use bitflags::bitflags;
use indexmap::IndexSet;
use itertools::Itertools;
use python_ast::{AnyNodeRef, Expr, Number, Operator, UnaryOp};
use python_ast_utils::{
    nodes::{NodeId, NodeStack, NodeWithParent, Nodes},
    ReturnStmtCollector,
};
use python_utils::is_python_module;
use ruff_text_size::Ranged;

use crate::{
    db::{FileId, SymbolTableDb},
    declaration::{Declaration, DeclarationKind, DeclarationQuery, ImportSource},
    mro::compute_mro,
    symbol::SymbolId,
    ScopeId,
};

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ResolvedType {
    KnownType(PythonType),
    Union(Vec<ResolvedType>),
    Any,
    Unknown,
    TypeError,
}

impl ResolvedType {
    pub fn into_class(self) -> Option<ClassType> {
        match self {
            Self::KnownType(PythonType::Class(class)) => Some(class),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum KnownClass {
    // Builtins
    Bool,
    Object,
    Bytes,
    Type,
    Int,
    Float,
    Complex,
    Str,
    List,
    Tuple,
    Set,
    FrozenSet,
    Dict,
    // typing/collection
    Counter,
    DefaultDict,
    Deque,
    ChainMap,
    OrderedDict,
}

impl KnownClass {
    fn from_symbol(name: &str) -> Option<KnownClass> {
        Some(match name {
            "bool" => Self::Bool,
            "object" => Self::Object,
            "bytes" => Self::Bytes,
            "tuple" => Self::Tuple,
            "type" => Self::Type,
            "int" => Self::Int,
            "float" => Self::Float,
            "complex" => Self::Complex,
            "str" => Self::Str,
            "set" => Self::Set,
            "frozenset" => Self::Set,
            "dict" => Self::Dict,
            "list" => Self::List,
            "Counter" => Self::Counter,
            "defaultdict" => Self::DefaultDict,
            "deque" => Self::Deque,
            "ChainMap" => Self::ChainMap,
            "OrderedDict" => Self::OrderedDict,
            _ => return None,
        })
    }

    fn create_builtin_type(db: &SymbolTableDb, known: KnownClass) -> ClassType {
        let declaration = db
            .builtin_symbols()
            .symbol_declaration(known.as_str())
            .expect("class declaration not found");
        ClassType {
            file_id: db.indexer().file_id(db.builtin_symbols().path()),
            node_id: declaration.node_id,
            symbol_id: declaration.symbol_id,
            body_scope: declaration.body_scope().unwrap(),
            known: Some(known),
        }
    }

    fn create_collection_type(db: &SymbolTableDb, known: KnownClass) -> ClassType {
        let declaration = db
            .collection_stub_symbol_table()
            .symbol_declaration(known.as_str(), ScopeId::global(), DeclarationQuery::Last)
            .expect("collection class declaration not found");
        ClassType {
            file_id: db.indexer().file_id(db.collection_stub_path()),
            node_id: declaration.node_id,
            symbol_id: declaration.symbol_id,
            body_scope: declaration.body_scope().unwrap(),
            known: Some(known),
        }
    }

    pub(crate) fn object(db: &SymbolTableDb) -> ClassType {
        KnownClass::create_builtin_type(db, KnownClass::Object)
    }

    fn list(db: &SymbolTableDb) -> ClassType {
        KnownClass::create_builtin_type(db, KnownClass::List)
    }

    fn dict(db: &SymbolTableDb) -> ClassType {
        KnownClass::create_builtin_type(db, KnownClass::Dict)
    }

    fn set(db: &SymbolTableDb) -> ClassType {
        KnownClass::create_builtin_type(db, KnownClass::Set)
    }

    fn tuple(db: &SymbolTableDb) -> ClassType {
        KnownClass::create_builtin_type(db, KnownClass::Tuple)
    }

    fn as_str(&self) -> &str {
        match self {
            KnownClass::Bool => "bool",
            KnownClass::Object => "object",
            KnownClass::Bytes => "bytes",
            KnownClass::Type => "type",
            KnownClass::Int => "int",
            KnownClass::Float => "float",
            KnownClass::Complex => "complex",
            KnownClass::Str => "str",
            KnownClass::List => "list",
            KnownClass::Tuple => "tuple",
            KnownClass::Set => "set",
            KnownClass::FrozenSet => "frozenset",
            KnownClass::Dict => "dict",
            KnownClass::Counter => "Counter",
            KnownClass::DefaultDict => "defaultdict",
            KnownClass::Deque => "deque",
            KnownClass::ChainMap => "ChainMap",
            KnownClass::OrderedDict => "OrderedDict",
        }
    }
}

// TODO: add type for class instance and class type
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PythonType {
    Module(ImportSource),
    Number(NumberLike),
    String,
    Bytes,
    None,
    Ellipsis,
    Generator,
    /// instance of the class
    Class(ClassType),
    // TODO: implement this case
    ParameterizedClass,
    Function {
        file_id: FileId,
        symbol_id: SymbolId,
        node_id: NodeId,
        body_scope: ScopeId,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ClassBase {
    Unknown,
    Class(ClassType),
}

impl ClassBase {
    pub(crate) fn try_from_type(ty: &ResolvedType) -> Option<ClassBase> {
        match ty {
            ResolvedType::KnownType(PythonType::Class(class)) => Some(ClassBase::Class(*class)),
            _ => None,
        }
    }

    pub fn into_class(self) -> Option<ClassType> {
        match self {
            ClassBase::Class(class) => Some(class),
            ClassBase::Unknown => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct ClassType {
    pub file_id: FileId,
    pub node_id: NodeId,
    pub symbol_id: SymbolId,
    pub body_scope: ScopeId,
    pub known: Option<KnownClass>,
}

impl ClassType {
    pub(crate) fn is_object(&self) -> bool {
        matches!(self.known, Some(KnownClass::Object))
    }

    pub fn node_stack<'db>(&self, db: &'db SymbolTableDb) -> NodeStack<'db> {
        db.indexer()
            .node_stack(db.indexer().file_path(&self.file_id))
    }

    pub fn class_bases(&self, db: &SymbolTableDb) -> Vec<ResolvedType> {
        let path = db.indexer().file_path(&self.file_id);
        let symbol = db.symbol(path, self.symbol_id);
        let declaration = db.declaration(path, symbol.declarations().last());

        let node_stack = if symbol.is_builtin() {
            db.builtin_symbols().node_stack()
        } else {
            db.indexer().node_stack(path)
        };

        let node_with_parent = node_stack.nodes().get(declaration.node_id).unwrap();
        match node_with_parent.node() {
            AnyNodeRef::StmtClassDef(python_ast::ClassDefStmt {
                arguments: Some(arguments),
                ..
            }) => {
                let mut type_inferer = TypeInferer::new(db, ScopeId::global(), path.clone());

                arguments
                    .args
                    .iter()
                    .map(|arg| type_inferer.infer_expr(arg, node_stack.nodes()))
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    pub(crate) fn is_cyclically_defined(&self, db: &SymbolTableDb) -> bool {
        fn is_cyclically_defined_recursive(
            db: &SymbolTableDb,
            class: ClassType,
            classes_to_watch: &mut IndexSet<ClassType>,
        ) -> bool {
            if !classes_to_watch.insert(class) {
                return true;
            }
            for explicit_base_class in class
                .class_bases(db)
                .into_iter()
                .filter_map(|ty| ty.into_class())
            {
                // Each base must be considered in isolation.
                // This is due to the fact that if a class uses multiple inheritance,
                // there could easily be a situation where two bases have the same class in their MROs;
                // that isn't enough to constitute the class being cyclically defined.
                let classes_to_watch_len = classes_to_watch.len();
                if is_cyclically_defined_recursive(db, explicit_base_class, classes_to_watch) {
                    return true;
                }
                classes_to_watch.truncate(classes_to_watch_len);
            }
            false
        }

        self.class_bases(db)
            .into_iter()
            .filter_map(|ty| ty.into_class())
            .any(|base_class| is_cyclically_defined_recursive(db, base_class, &mut IndexSet::new()))
    }

    pub fn constructor<'db>(&self, db: &'db SymbolTableDb) -> Option<&'db Declaration> {
        let file_path = db.indexer().file_path(&self.file_id);
        // TODO: handle multiple constructors
        db.symbol_declaration(
            file_path,
            "__init__",
            self.body_scope,
            DeclarationQuery::First,
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum NumberLike {
    Int,
    Float,
    Complex,
    Bool,
}

impl NumberLike {
    /// Coerces two number-like types to the "highest" number-like type.
    #[must_use]
    pub fn coerce(self, other: NumberLike) -> NumberLike {
        match (self, other) {
            (NumberLike::Complex, _) | (_, NumberLike::Complex) => NumberLike::Complex,
            (NumberLike::Float, _) | (_, NumberLike::Float) => NumberLike::Float,
            _ => NumberLike::Int,
        }
    }
}

impl PythonType {
    fn is_subtype_of(&self, other: &PythonType) -> bool {
        match (self, other) {
            (PythonType::String, PythonType::String) => true,
            (PythonType::Bytes, PythonType::Bytes) => true,
            (PythonType::None, PythonType::None) => true,
            (PythonType::Ellipsis, PythonType::Ellipsis) => true,
            // The Numeric Tower (https://peps.python.org/pep-3141/)
            (PythonType::Number(NumberLike::Bool), PythonType::Number(NumberLike::Bool)) => true,
            (PythonType::Number(NumberLike::Int), PythonType::Number(NumberLike::Int)) => true,
            (PythonType::Number(NumberLike::Float), PythonType::Number(NumberLike::Float)) => true,
            (PythonType::Number(NumberLike::Complex), PythonType::Number(NumberLike::Complex)) => {
                true
            }
            (PythonType::Number(NumberLike::Bool), PythonType::Number(NumberLike::Int)) => true,
            (PythonType::Number(NumberLike::Bool), PythonType::Number(NumberLike::Float)) => true,
            (PythonType::Number(NumberLike::Bool), PythonType::Number(NumberLike::Complex)) => true,
            (PythonType::Number(NumberLike::Int), PythonType::Number(NumberLike::Float)) => true,
            (PythonType::Number(NumberLike::Int), PythonType::Number(NumberLike::Complex)) => true,
            (PythonType::Number(NumberLike::Float), PythonType::Number(NumberLike::Complex)) => {
                true
            }
            _ => false,
        }
    }

    #[allow(dead_code)]
    fn display(&self, db: &SymbolTableDb) -> String {
        match self {
            PythonType::Module(import_source) => {
                format!(
                    "Module[{}]",
                    import_source
                        .any_path()
                        .map(|path| path.display().to_string())
                        .unwrap_or("Unresolved".into())
                )
            }
            PythonType::Number(number) => match number {
                NumberLike::Int => "int".into(),
                NumberLike::Float => "float".into(),
                NumberLike::Complex => "complex".into(),
                NumberLike::Bool => "bool".into(),
            },
            PythonType::String => "str".into(),
            PythonType::Bytes => "bytes".into(),
            PythonType::None => "None".into(),
            PythonType::Ellipsis => "ellipsis".into(),
            PythonType::Generator => "Generator".into(),
            PythonType::Function { .. } => "function".into(),
            PythonType::Class(ClassType {
                file_id, symbol_id, ..
            }) => {
                let path = db.indexer().file_path(file_id);
                let symbol = db.symbol(path, *symbol_id);
                let scope = db.scope(path, symbol.definition_scope());
                let name = scope.symbol_name(*symbol_id).unwrap();
                format!("class[{name}]")
            }
            PythonType::ParameterizedClass => todo!(),
        }
    }
}

impl ResolvedType {
    fn union(self, other: ResolvedType) -> ResolvedType {
        match (self, other) {
            (Self::TypeError, _) | (_, Self::TypeError) => Self::TypeError,
            (Self::Any, _) | (_, Self::Any) => Self::Any,
            (Self::Unknown, _) | (_, Self::Unknown) => Self::Unknown,
            (a @ Self::KnownType(_), b @ Self::KnownType(_)) => {
                if a.is_subtype_of(&b) {
                    b
                } else if b.is_subtype_of(&a) {
                    a
                } else {
                    Self::Union(vec![a, b])
                }
            }
            (a @ Self::KnownType(_), Self::Union(mut b)) => {
                // If `a` is a subtype of any of the types in `b`, then `a` is
                // redundant.
                if !b.iter().any(|b_element| a.is_subtype_of(b_element)) {
                    b.push(a);
                }
                Self::Union(b)
            }
            (Self::Union(mut a), b @ Self::KnownType(_)) => {
                // If `b` is a subtype of any of the types in `a`, then `b` is
                // redundant.
                if !a.iter().any(|a_element| b.is_subtype_of(a_element)) {
                    a.push(b);
                }
                Self::Union(a)
            }
            (Self::Union(mut a), Self::Union(b)) => {
                for b_element in b {
                    // If `b_element` is a subtype of any of the types in `a`, then
                    // `b_element` is redundant.
                    if !a.iter().any(|a_element| b_element.is_subtype_of(a_element)) {
                        a.push(b_element);
                    }
                }
                Self::Union(a)
            }
        }
    }

    fn is_subtype_of(&self, other: &ResolvedType) -> bool {
        match (self, other) {
            (
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::Dict),
                    ..
                })),
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::Dict),
                    ..
                })),
            ) => true,
            (
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::List),
                    ..
                })),
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::List),
                    ..
                })),
            ) => true,
            (
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::Set),
                    ..
                })),
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::Set),
                    ..
                })),
            ) => true,
            (
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::Tuple),
                    ..
                })),
                Self::KnownType(PythonType::Class(ClassType {
                    known: Some(KnownClass::Tuple),
                    ..
                })),
            ) => true,
            (Self::KnownType(py_type), Self::KnownType(other_py_type)) => {
                py_type.is_subtype_of(other_py_type)
            }
            _ => false,
        }
    }

    #[allow(dead_code)]
    fn display(&self, db: &SymbolTableDb) -> String {
        match self {
            ResolvedType::KnownType(python_type) => python_type.display(db),
            ResolvedType::Union(types) => types.iter().map(|type_| type_.display(db)).join(" | "),
            ResolvedType::Any => "any".into(),
            ResolvedType::Unknown => "Unknow".into(),
            ResolvedType::TypeError => "TypeError".into(),
        }
    }
}

fn infer_literal_expr_type(literal: &AnyNodeRef) -> ResolvedType {
    let python_type = match literal {
        AnyNodeRef::NoneLiteralExpr(_) => PythonType::None,
        AnyNodeRef::BooleanLiteralExpr(_) => PythonType::Number(NumberLike::Bool),
        AnyNodeRef::StringLiteralExpr(_) | AnyNodeRef::FStringExpr(_) => PythonType::String,
        AnyNodeRef::BytesLiteralExpr(_) => PythonType::Bytes,
        AnyNodeRef::EllipsisLiteralExpr(_) => PythonType::Ellipsis,
        AnyNodeRef::NumberLiteralExpr(python_ast::NumberLiteralExpr { value, .. }) => match value {
            Number::Int(_) => PythonType::Number(NumberLike::Int),
            Number::Float(_) => PythonType::Number(NumberLike::Float),
            Number::Complex { .. } => PythonType::Number(NumberLike::Complex),
        },
        _ => unreachable!("expression is not a literal"),
    };

    ResolvedType::KnownType(python_type)
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    struct TypeInferFlags: u8 {
        const IN_CALL_EXPR = 1 << 0;
    }
}

pub struct TypeInferer<'db> {
    db: &'db SymbolTableDb,
    path: Arc<PathBuf>,
    curr_scope: ScopeId,
    flag_stack: Vec<TypeInferFlags>,
}

impl<'db> TypeInferer<'db> {
    pub fn new(db: &'db SymbolTableDb, scope_id: ScopeId, path: Arc<PathBuf>) -> Self {
        Self {
            db,
            path,
            curr_scope: scope_id,
            flag_stack: vec![TypeInferFlags::empty()],
        }
    }

    fn set_flag(&mut self, flag: TypeInferFlags) {
        let flags = self.flag_stack.last_mut().unwrap();
        if flags.contains(flag) {
            self.flag_stack.push(flag);
        } else {
            flags.insert(flag);
        }
    }

    fn remove_flag(&mut self, flag: TypeInferFlags) {
        let flags = self.flag_stack.last_mut().unwrap();
        flags.remove(flag);
        if flags.is_empty() && self.flag_stack.len() > 1 {
            self.flag_stack.pop();
        }
    }

    fn contains_flag(&mut self, flag: TypeInferFlags) -> bool {
        self.flag_stack
            .last()
            .is_some_and(|flags| flags.contains(flag))
    }

    pub fn set_scope(&mut self, scope_id: ScopeId) {
        self.curr_scope = scope_id;
    }

    fn infer_in_file<F, R>(&mut self, new_path: Arc<PathBuf>, f: F) -> R
    where
        F: FnOnce(&mut Self, &Nodes) -> R,
    {
        // Store the original context
        let original_path = self.path.clone();
        let original_scope = self.curr_scope;

        let node_stack = self.db.indexer().node_stack(&new_path);
        let nodes = node_stack.nodes();

        // Change to new context
        self.path = new_path;
        self.curr_scope = ScopeId::global();

        // Execute the provided function with the new context
        let result = f(self, nodes);

        // Restore the original context
        self.path = original_path;
        self.curr_scope = original_scope;

        result
    }

    fn infer_in_scope<F, R>(&mut self, path: Arc<PathBuf>, scope_id: ScopeId, func: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        // Store the original context
        let original_path = self.path.clone();
        let original_scope = self.curr_scope;

        // Change to new context
        self.path = path;
        self.curr_scope = scope_id;
        let result = func(self);
        // Restore the original context
        self.path = original_path;
        self.curr_scope = original_scope;

        result
    }

    pub fn infer_node(&mut self, node: &NodeWithParent, nodes: &Nodes) -> ResolvedType {
        match node.node() {
            AnyNodeRef::StmtAssign(python_ast::AssignStmt { targets, value, .. }) => {
                let [target, ..] = targets.as_slice() else {
                    unreachable!()
                };
                // Special case for the collections types (`List`, `Set`, `Tuple`, `Dict`, `DefaultDict`,
                // `FrozenSet`, `Counter`, `Deque`, `ChainMap`) defined in the `typing.pyi` file
                if self.path.ends_with("stdlib/typing.pyi") && is_collection_typing(target) {
                    return self.handle_typing_stub_assignment_or_annotation(**node);
                }

                self.infer_expr(value.as_ref(), nodes)
            }
            AnyNodeRef::StmtAnnAssign(python_ast::AnnAssignStmt {
                annotation, value, ..
            }) => {
                if self.path.ends_with("stdlib/typing.pyi")
                    && matches!(annotation.as_ref(), Expr::Name(python_ast::NameExpr {id, ..}) if id == "_SpecialForm")
                {
                    return self.handle_typing_stub_assignment_or_annotation(**node);
                }

                if let Some(value) = value {
                    self.infer_expr(value.as_ref(), nodes)
                } else {
                    self.infer_expr(annotation.as_ref(), nodes)
                }
            }
            AnyNodeRef::ExprStmt(python_ast::ExprStmt { value, .. }) => {
                self.infer_expr(value.as_ref(), nodes)
            }
            AnyNodeRef::Comprehension(python_ast::Comprehension { iter, .. }) => {
                self.infer_expr(iter, nodes)
            }
            AnyNodeRef::Parameter(python_ast::Parameter { name, .. })
            | AnyNodeRef::Alias(python_ast::Alias { name, .. })
            | AnyNodeRef::StmtClassDef(python_ast::ClassDefStmt { name, .. })
            | AnyNodeRef::StmtFunctionDef(python_ast::FunctionDefStmt { name, .. }) => {
                self.infer_symbol(name, nodes, DeclarationQuery::First)
            }
            AnyNodeRef::StmtImportFrom(python_ast::ImportFromStmt {
                module: Some(module),
                ..
            }) => self.infer_symbol(module, nodes, DeclarationQuery::First),
            _ => self.infer_expr(**node, nodes),
        }
    }

    fn handle_typing_stub_assignment_or_annotation(&mut self, node: AnyNodeRef) -> ResolvedType {
        match node {
            AnyNodeRef::StmtAssign(python_ast::AssignStmt { targets, .. }) => {
                let [target, ..] = targets.as_slice() else {
                    unreachable!()
                };

                let collection_name = &target.as_name_expr().expect("name expression").id;
                let class_type = match collection_name.as_str() {
                    "List" => KnownClass::list(self.db),
                    "Tuple" => KnownClass::tuple(self.db),
                    "Set" => KnownClass::set(self.db),
                    "Dict" => KnownClass::tuple(self.db),
                    "FrozenSet" => KnownClass::create_builtin_type(self.db, KnownClass::FrozenSet),
                    "Counter" => KnownClass::create_collection_type(self.db, KnownClass::Counter),
                    "DefaultDict" => {
                        KnownClass::create_collection_type(self.db, KnownClass::DefaultDict)
                    }
                    "Deque" => KnownClass::create_collection_type(self.db, KnownClass::Deque),
                    "ChainMap" => KnownClass::create_collection_type(self.db, KnownClass::ChainMap),
                    "OrderedDict" => {
                        KnownClass::create_collection_type(self.db, KnownClass::OrderedDict)
                    }
                    _ => unreachable!(),
                };

                ResolvedType::KnownType(PythonType::Class(class_type))
            }
            AnyNodeRef::StmtAnnAssign(python_ast::AnnAssignStmt { target, range, .. }) => {
                let special_type_name = &target.as_name_expr().expect("name expression").id;
                let special_type_declaration = self
                    .db
                    .symbol_declaration(
                        &self.path,
                        special_type_name,
                        self.curr_scope,
                        DeclarationQuery::AtOffset(range.start().to_u32()),
                    )
                    .unwrap();
                let special_form_declaration = self
                    .db
                    .symbol_declaration(
                        &self.path,
                        "_SpecialForm",
                        self.curr_scope,
                        DeclarationQuery::Last,
                    )
                    .unwrap();

                ResolvedType::KnownType(PythonType::Class(ClassType {
                    file_id: self.db.indexer().file_id(&self.path),
                    node_id: special_form_declaration.node_id,
                    symbol_id: special_type_declaration.symbol_id,
                    body_scope: special_form_declaration.body_scope().unwrap(),
                    known: None,
                }))
            }
            _ => unreachable!(),
        }
    }

    pub fn infer_expr<'a>(
        &mut self,
        expr: impl Into<AnyNodeRef<'a>>,
        nodes: &Nodes,
    ) -> ResolvedType {
        let expr = expr.into();
        match expr {
            AnyNodeRef::NameExpr(python_ast::NameExpr { id, range, .. }) => self.infer_symbol(
                id,
                nodes,
                DeclarationQuery::AtOffset(range.start().to_u32()),
            ),
            AnyNodeRef::CallExpr(python_ast::CallExpr { func, .. }) => {
                self.set_flag(TypeInferFlags::IN_CALL_EXPR);
                let result = self.infer_expr(func.as_ref(), nodes);
                self.remove_flag(TypeInferFlags::IN_CALL_EXPR);
                result
            }
            AnyNodeRef::ListExpr(_) | AnyNodeRef::ListCompExpr(_) => {
                ResolvedType::KnownType(PythonType::Class(KnownClass::list(self.db)))
            }
            AnyNodeRef::TupleExpr(_) => {
                ResolvedType::KnownType(PythonType::Class(KnownClass::tuple(self.db)))
            }
            AnyNodeRef::SetExpr(_) | AnyNodeRef::SetCompExpr(_) => {
                ResolvedType::KnownType(PythonType::Class(KnownClass::set(self.db)))
            }
            AnyNodeRef::DictExpr(_) | AnyNodeRef::DictCompExpr(_) => {
                ResolvedType::KnownType(PythonType::Class(KnownClass::dict(self.db)))
            }
            AnyNodeRef::NamedExpr(python_ast::NamedExpr { value, .. }) => {
                self.infer_expr(value.as_ref(), nodes)
            }
            AnyNodeRef::IfExpr(python_ast::IfExpr { body, orelse, .. }) => {
                let body = self.infer_expr(body.as_ref(), nodes);
                let orelse = self.infer_expr(orelse.as_ref(), nodes);

                body.union(orelse)
            }
            AnyNodeRef::BoolOpExpr(python_ast::BoolOpExpr { values, .. }) => values
                .iter()
                .map(|expr| self.infer_expr(expr, nodes))
                .reduce(ResolvedType::union)
                .unwrap_or(ResolvedType::Unknown),
            AnyNodeRef::UnaryOpExpr(python_ast::UnaryOpExpr { op, operand, .. }) => match op {
                UnaryOp::Invert => match self.infer_expr(operand.as_ref(), nodes) {
                    ResolvedType::KnownType(PythonType::Number(
                        NumberLike::Bool | NumberLike::Int,
                    )) => ResolvedType::KnownType(PythonType::Number(NumberLike::Int)),
                    ResolvedType::KnownType(_) => ResolvedType::TypeError,
                    _ => ResolvedType::Unknown,
                },
                UnaryOp::Not => ResolvedType::KnownType(PythonType::Number(NumberLike::Bool)),
                UnaryOp::UAdd | UnaryOp::USub => match self.infer_expr(operand.as_ref(), nodes) {
                    ResolvedType::KnownType(PythonType::Number(number)) => {
                        ResolvedType::KnownType(PythonType::Number(if number == NumberLike::Bool {
                            NumberLike::Int
                        } else {
                            number
                        }))
                    }
                    ResolvedType::KnownType(_) => ResolvedType::TypeError,
                    _ => ResolvedType::Unknown,
                },
            },
            AnyNodeRef::BinOpExpr(python_ast::BinOpExpr {
                left, op, right, ..
            }) => self.infer_bin_op(left, op, right, nodes),
            AnyNodeRef::GeneratorExpExpr(_) => ResolvedType::KnownType(PythonType::Generator),
            AnyNodeRef::AttributeExpr(python_ast::AttributeExpr { value, attr, .. }) => {
                let value_type = self.infer_expr(value.as_ref(), nodes);

                match value_type {
                    ResolvedType::KnownType(PythonType::Class(class)) => {
                        // TODO: how do we upstream this error?
                        let Ok(mro_result) = compute_mro(self.db, class) else {
                            return ResolvedType::Unknown;
                        };
                        let Some((path, declaration)) = mro_result
                            .into_iter()
                            .filter_map(|class_base| class_base.into_class())
                            .find_map(|class| {
                                let path = self.db.indexer().file_path(&class.file_id);
                                self.db
                                    .symbol_declaration(
                                        path,
                                        attr,
                                        class.body_scope,
                                        DeclarationQuery::Last,
                                    )
                                    .map(|decl| (path, decl))
                            })
                        else {
                            return ResolvedType::Unknown;
                        };

                        let definition_scope = self
                            .db
                            .symbol(path, declaration.symbol_id)
                            .definition_scope();
                        let node_stack = self.db.indexer().node_stack(path);
                        let nodes = node_stack.nodes();

                        self.infer_in_scope(path.clone(), definition_scope, |this| {
                            this.resolve_declaration_type(declaration, nodes)
                        })
                    }
                    ResolvedType::KnownType(PythonType::Module(import_source)) => {
                        let Some(module_path) = import_source.any_path() else {
                            return ResolvedType::Unknown;
                        };

                        self.infer_in_file(module_path.clone(), |this, nodes| {
                            this.infer_symbol(attr, nodes, DeclarationQuery::Last)
                        })
                    }
                    _ => value_type,
                }
            }
            AnyNodeRef::SubscriptExpr(python_ast::SubscriptExpr { value, slice, .. }) => {
                let mut value_type = self.infer_expr(value.as_ref(), nodes);
                let ResolvedType::KnownType(PythonType::Class(ClassType {
                    file_id,
                    symbol_id,
                    ..
                })) = &mut value_type
                else {
                    return ResolvedType::Unknown;
                };

                let slice_type = if matches!(slice.as_ref(), Expr::Name(python_ast::NameExpr {id, ..}) if id.is_empty())
                {
                    vec![ResolvedType::Unknown]
                } else {
                    self.resolve_inner_collection_type(slice, nodes)
                };

                let symbol_name = self
                    .db
                    .symbol_name(self.db.indexer().file_path(file_id), *symbol_id);
                if symbol_name == "Union" {
                    ResolvedType::Union(slice_type)
                } else {
                    value_type
                }
            }
            AnyNodeRef::NoneLiteralExpr(_)
            | AnyNodeRef::BooleanLiteralExpr(_)
            | AnyNodeRef::StringLiteralExpr(_)
            | AnyNodeRef::FStringExpr(_)
            | AnyNodeRef::BytesLiteralExpr(_)
            | AnyNodeRef::NumberLiteralExpr(_)
            | AnyNodeRef::EllipsisLiteralExpr(_) => infer_literal_expr_type(&expr),
            _ => ResolvedType::Unknown,
        }
    }

    fn resolve_inner_collection_type(&mut self, expr: &Expr, nodes: &Nodes) -> Vec<ResolvedType> {
        let mut types = Vec::new();

        let mut func = |expr: &Expr| match expr {
            Expr::BinOp(python_ast::BinOpExpr {
                op: Operator::BitOr,
                ..
            }) => {
                let exprs = collect_bitor_exprs(expr);
                exprs
                    .into_iter()
                    .map(|expr| {
                        if matches!(expr, Expr::Name(_) | Expr::Subscript(_)) {
                            self.infer_expr(expr, nodes)
                        } else {
                            ResolvedType::Unknown
                        }
                    })
                    .reduce(ResolvedType::union)
                    .unwrap_or(ResolvedType::Unknown)
            }
            _ => self.infer_expr(expr, nodes),
        };

        if let Expr::Tuple(python_ast::TupleExpr { elts, .. }) = expr {
            for element in elts {
                types.push(func(element));
            }
        } else {
            types.push(func(expr));
        }

        types
    }

    fn resolve_declaration_type(
        &mut self,
        declaration: &'db Declaration,
        nodes: &Nodes,
    ) -> ResolvedType {
        let declaration_node = nodes.get(declaration.node_id).expect("declaration node");
        match declaration_node.node() {
            AnyNodeRef::StmtClassDef(python_ast::ClassDefStmt { name, .. }) => {
                ResolvedType::KnownType(PythonType::Class(ClassType {
                    file_id: declaration.file_id,
                    node_id: declaration.node_id,
                    symbol_id: declaration.symbol_id,
                    body_scope: declaration.body_scope().unwrap(),
                    known: KnownClass::from_symbol(name),
                }))
            }
            AnyNodeRef::StmtFunctionDef(python_ast::FunctionDefStmt { body, returns, .. }) => {
                if !self.contains_flag(TypeInferFlags::IN_CALL_EXPR) {
                    return ResolvedType::KnownType(PythonType::Function {
                        file_id: declaration.file_id,
                        symbol_id: declaration.symbol_id,
                        body_scope: declaration.body_scope().unwrap(),
                        node_id: declaration.node_id,
                    });
                }

                // If a return type annotation exists (e.g., `def foo() -> int:`),
                // use it as the explicit return type of the function instead of
                // inferring from the function body
                if let Some(returns) = returns {
                    return self.infer_expr(returns.as_ref(), nodes);
                }

                // Otherwise, infer the return type by analyzing all return statements:
                // 1. Collect every `return` statement in the function body
                // 2. Extract the returned expressions (ignoring bare `return` statements)
                // 3. Infer the type of each returned expression
                // 4. Create a union type of all possible return types
                // 5. If no return statements exist, default to Any type
                let mut return_collector = ReturnStmtCollector::default();
                let return_stmts = return_collector.collect(body);

                return_stmts
                    .iter()
                    .filter_map(|stmt| stmt.value.as_ref())
                    .map(|expr| self.infer_expr(expr.as_ref(), nodes))
                    .reduce(ResolvedType::union)
                    .unwrap_or(ResolvedType::Any)
            }
            AnyNodeRef::Alias(python_ast::Alias { name, .. }) => {
                let Some(import_source) = declaration.import_source() else {
                    return ResolvedType::Unknown;
                };

                let import_stmt = declaration_node
                    .parent_id()
                    .and_then(|node_id| nodes.get(node_id))
                    .unwrap();

                self.infer_imported_symbol(import_stmt, name, import_source)
                    .unwrap_or(ResolvedType::Unknown)
            }
            AnyNodeRef::StmtImportFrom(python_ast::ImportFromStmt {
                module: Some(module),
                ..
            }) => {
                let Some(import_source) = declaration.import_source() else {
                    return ResolvedType::Unknown;
                };

                self.infer_imported_symbol(declaration_node, module, import_source)
                    .unwrap_or(ResolvedType::Unknown)
            }
            AnyNodeRef::Parameter(python_ast::Parameter { annotation, .. }) => {
                // Handle instance parameters in class methods (e.g., `self` in `def method(self)`)
                // When a parameter is marked as an instance parameter:
                // 1. It represents the class instance itself (e.g., `self` refers to the class)
                // 2. Look up the class's symbol to get its scope and definition
                // 3. Return a resolved type pointing to the class definition
                // Example:
                //   class MyClass:
                //       def method(self): ... # <- `self` has type `MyClass`
                if let DeclarationKind::InstanceParameter(class_symbol_id) = declaration.kind {
                    let class_symbol = self.db.symbol(&self.path, class_symbol_id);
                    let declaration = self
                        .db
                        .declaration(&self.path, class_symbol.declarations().last());
                    return self.resolve_declaration_type(declaration, nodes);
                }

                if let Some(annotation) = annotation {
                    return self.infer_expr(annotation.as_ref(), nodes);
                }

                if let Some(AnyNodeRef::ParameterWithDefault(python_ast::ParameterWithDefault {
                    default: Some(default),
                    ..
                })) = nodes
                    .get(declaration_node.parent_id().unwrap())
                    .map(|node| node.node())
                {
                    return self.infer_expr(default.as_ref(), nodes);
                }

                ResolvedType::Any
            }
            AnyNodeRef::PatternMatchAs(_) => {
                self.infer_pattern_match_as_type(declaration_node, nodes)
            }
            _ => self.infer_node(declaration_node, nodes),
        }
    }

    fn infer_pattern_match_as_type(
        &mut self,
        node: &NodeWithParent,
        nodes: &Nodes,
    ) -> ResolvedType {
        let Some(parent_node) = nodes.get(node.parent_id().expect("parent node for pattern match"))
        else {
            unreachable!()
        };

        match parent_node.node() {
            AnyNodeRef::StmtMatch(python_ast::MatchStmt { subject, .. }) => {
                self.infer_expr(subject.as_ref(), nodes)
            }
            // TODO: in a `PatternMatchSequence`, if the `MatchStmt` subject is a collection,
            // the inferred type should be the elements type of the collection.
            AnyNodeRef::PatternMatchSequence(python_ast::PatternMatchSequence { .. }) => {
                self.infer_pattern_match_as_type(parent_node, nodes)
            }
            _ => ResolvedType::Unknown,
        }
    }

    fn infer_imported_symbol(
        &mut self,
        node: &AnyNodeRef,
        name: &str,
        import_source: &'db ImportSource,
    ) -> Option<ResolvedType> {
        if import_source.is_unresolved() {
            return None;
        }
        Some(match node {
            AnyNodeRef::StmtImport(_) => {
                ResolvedType::KnownType(PythonType::Module(import_source.clone()))
            }
            AnyNodeRef::StmtImportFrom(python_ast::ImportFromStmt { .. }) => {
                let path = import_source.any_path().unwrap();
                if is_python_module(name, path) {
                    ResolvedType::KnownType(PythonType::Module(import_source.clone()))
                } else {
                    self.infer_in_file(path.clone(), |this, nodes| {
                        this.infer_symbol(name, nodes, DeclarationQuery::Last)
                    })
                }
            }
            _ => unreachable!(),
        })
    }

    pub fn infer_symbol(
        &mut self,
        name: &str,
        nodes: &Nodes,
        query: DeclarationQuery,
    ) -> ResolvedType {
        let Some(declaration) =
            self.db
                .symbol_declaration(&self.path, name, self.curr_scope, query)
        else {
            return ResolvedType::Unknown;
        };

        if self
            .db
            .symbol(&self.path, declaration.symbol_id)
            .is_builtin()
        {
            return self.infer_in_file(
                self.db.builtin_symbols().path().clone(),
                |this, builtin_nodes| this.resolve_declaration_type(declaration, builtin_nodes),
            );
        }

        self.resolve_declaration_type(declaration, nodes)
    }

    fn infer_bin_op(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        nodes: &Nodes,
    ) -> ResolvedType {
        let left_type = self.infer_expr(left, nodes);
        let right_type = self.infer_expr(right, nodes);
        match op {
            Operator::Add => {
                match (left_type, right_type) {
                    // Ex) `"Hello" + "world"`
                    (
                        ResolvedType::KnownType(PythonType::String),
                        ResolvedType::KnownType(PythonType::String),
                    ) => return ResolvedType::KnownType(PythonType::String),
                    // Ex) `b"Hello" + b"world"`
                    (
                        ResolvedType::KnownType(PythonType::Bytes),
                        ResolvedType::KnownType(PythonType::Bytes),
                    ) => return ResolvedType::KnownType(PythonType::Bytes),
                    // Ex) `[1] + [2]`
                    (
                        ResolvedType::KnownType(PythonType::Class(ClassType {
                            file_id,
                            node_id,
                            symbol_id,
                            body_scope,
                            known: Some(KnownClass::List),
                            ..
                        })),
                        ResolvedType::KnownType(PythonType::Class(ClassType {
                            known: Some(KnownClass::List),
                            ..
                        })),
                    ) => {
                        return ResolvedType::KnownType(PythonType::Class(ClassType {
                            file_id,
                            node_id,
                            symbol_id,
                            body_scope,
                            known: Some(KnownClass::List),
                        }));
                    }
                    // Ex) `(1, 2) + (3, 4)`
                    (
                        ResolvedType::KnownType(PythonType::Class(ClassType {
                            file_id,
                            node_id,
                            symbol_id,
                            body_scope,
                            known: Some(KnownClass::Tuple),
                            ..
                        })),
                        ResolvedType::KnownType(PythonType::Class(ClassType {
                            known: Some(KnownClass::Tuple),
                            ..
                        })),
                    ) => {
                        return ResolvedType::KnownType(PythonType::Class(ClassType {
                            file_id,
                            node_id,
                            symbol_id,
                            body_scope,
                            known: Some(KnownClass::Tuple),
                        }));
                    }
                    // Ex) `1 + 1.0`
                    (
                        ResolvedType::KnownType(PythonType::Number(left)),
                        ResolvedType::KnownType(PythonType::Number(right)),
                    ) => {
                        return ResolvedType::KnownType(PythonType::Number(left.coerce(right)));
                    }
                    // Ex) `"a" + 1`
                    (ResolvedType::KnownType(_), ResolvedType::KnownType(_)) => {
                        return ResolvedType::TypeError;
                    }
                    _ => {}
                }
            }
            Operator::Sub => {
                match (left_type, right_type) {
                    // Ex) `1 - 1`
                    (
                        ResolvedType::KnownType(PythonType::Number(left)),
                        ResolvedType::KnownType(PythonType::Number(right)),
                    ) => {
                        return ResolvedType::KnownType(PythonType::Number(left.coerce(right)));
                    }
                    // Ex) `{1, 2} - {2}`
                    (
                        ResolvedType::KnownType(PythonType::Class(ClassType {
                            file_id,
                            node_id,
                            symbol_id,
                            body_scope,
                            known: Some(KnownClass::Set),
                            ..
                        })),
                        ResolvedType::KnownType(PythonType::Class(ClassType {
                            known: Some(KnownClass::Set),
                            ..
                        })),
                    ) => {
                        return ResolvedType::KnownType(PythonType::Class(ClassType {
                            file_id,
                            node_id,
                            symbol_id,
                            body_scope,
                            known: Some(KnownClass::Set),
                        }));
                    }
                    // Ex) `"a" - "b"`
                    (ResolvedType::KnownType(_), ResolvedType::KnownType(_)) => {
                        return ResolvedType::TypeError;
                    }
                    _ => {}
                }
            }
            // Ex) "a" % "b"
            Operator::Mod => match (left_type, right_type) {
                // Ex) `"Hello" % "world"`
                (ResolvedType::KnownType(PythonType::String), _) => {
                    return ResolvedType::KnownType(PythonType::String)
                }
                // Ex) `b"Hello" % b"world"`
                (ResolvedType::KnownType(PythonType::Bytes), _) => {
                    return ResolvedType::KnownType(PythonType::Bytes)
                }
                // Ex) `1 % 2`
                (
                    ResolvedType::KnownType(PythonType::Number(left)),
                    ResolvedType::KnownType(PythonType::Number(right)),
                ) => {
                    return ResolvedType::KnownType(PythonType::Number(left.coerce(right)));
                }
                _ => {}
            },
            // Standard arithmetic operators, which coerce to the "highest" number type.
            Operator::Mult | Operator::FloorDiv | Operator::Pow => match (left_type, right_type) {
                (
                    ResolvedType::KnownType(PythonType::Number(left)),
                    ResolvedType::KnownType(PythonType::Number(right)),
                ) => return ResolvedType::KnownType(PythonType::Number(left.coerce(right))),
                (ResolvedType::KnownType(_), ResolvedType::KnownType(_)) => {
                    return ResolvedType::TypeError;
                }
                _ => {}
            },
            // Division, which returns at least `float`.
            Operator::Div => match (left_type, right_type) {
                // Ex) `1 / 2`
                (
                    ResolvedType::KnownType(PythonType::Number(left)),
                    ResolvedType::KnownType(PythonType::Number(right)),
                ) => {
                    let resolved = left.coerce(right);
                    return ResolvedType::KnownType(PythonType::Number(
                        if resolved == NumberLike::Int {
                            NumberLike::Float
                        } else {
                            resolved
                        },
                    ));
                }
                (ResolvedType::KnownType(_), ResolvedType::KnownType(_)) => {
                    return ResolvedType::TypeError;
                }
                _ => {}
            },
            // Bitwise operators, which only work on `int` and `bool`.
            Operator::BitAnd
            | Operator::BitOr
            | Operator::BitXor
            | Operator::LShift
            | Operator::RShift => match (left_type, right_type) {
                // Ex) `1 & 2`
                (
                    ResolvedType::KnownType(PythonType::Number(left)),
                    ResolvedType::KnownType(PythonType::Number(right)),
                ) => {
                    let resolved = left.coerce(right);
                    return if resolved == NumberLike::Int {
                        ResolvedType::KnownType(PythonType::Number(NumberLike::Int))
                    } else {
                        ResolvedType::TypeError
                    };
                }
                (ResolvedType::KnownType(_), ResolvedType::KnownType(_)) => {
                    return ResolvedType::TypeError;
                }
                _ => {}
            },
            Operator::MatMult => {}
        }
        ResolvedType::Unknown
    }
}

fn collect_bitor_exprs(expr: &Expr) -> Vec<&Expr> {
    let mut exprs = Vec::new();
    match expr {
        Expr::BinOp(python_ast::BinOpExpr {
            left,
            op: Operator::BitOr,
            right,
            ..
        }) => {
            exprs.extend(collect_bitor_exprs(left));
            exprs.extend(collect_bitor_exprs(right));
        }
        _ => exprs.push(expr),
    };

    exprs
}

fn is_collection_typing(expr: &Expr) -> bool {
    let Expr::Name(python_ast::NameExpr { id, .. }) = expr else {
        return false;
    };

    matches!(
        id.as_str(),
        "List"
            | "Set"
            | "Tuple"
            | "Dict"
            | "DefaultDict"
            | "FrozenSet"
            | "Counter"
            | "Deque"
            | "ChainMap"
    )
}

#[cfg(test)]
mod tests {
    use std::{
        path::{Path, PathBuf},
        sync::Arc,
    };

    use python_ast::{AnyNodeRef, Expr};
    use python_utils::{interpreter::resolve_python_interpreter, PythonHost};
    use ruff_text_size::Ranged;

    use crate::{
        db::{Source, SymbolTableDb},
        type_inference::TypeInferer,
    };

    fn resolve_relative_path(path: impl AsRef<Path>) -> PathBuf {
        let curr_dir = std::env::current_dir().expect("Failed to get current dir");
        if path.as_ref().starts_with("..") || path.as_ref().starts_with(".") {
            let path: PathBuf = path
                .as_ref()
                .components()
                .filter(|c| {
                    !matches!(
                        c,
                        std::path::Component::ParentDir | std::path::Component::CurDir
                    )
                })
                .collect();
            curr_dir.join(path)
        } else {
            curr_dir.join(path)
        }
    }

    fn setup_db(src: &str, root: &Path, path: &Path) -> crate::db::SymbolTableDb {
        let interpreter = resolve_python_interpreter(root).expect("Valid python interpreter");
        let mut db = SymbolTableDb::new(root.to_path_buf(), PythonHost::new(interpreter))
            .with_builtin_symbols()
            .with_collection_types();
        db.indexer_mut()
            .add_or_update_file(path.to_path_buf(), Source::New(src));
        db
    }

    fn assert_type(src: &str, path: impl AsRef<Path>, expected: &str) {
        let path = Arc::new(resolve_relative_path(path));
        let parent = path.parent().unwrap();
        let curr_dir = std::env::current_dir().expect("Failed to get current dir");
        let root = if parent.to_string_lossy() == "" {
            &curr_dir
        } else {
            parent
        };
        let db = setup_db(src, root, &path);
        let stack = db.indexer().node_stack(&path);

        let reveal_type_func = stack.nodes().iter().find(|node_with_parent| {
            matches!(
                node_with_parent.node(),
                AnyNodeRef::CallExpr(python_ast::CallExpr { func, .. })
                    if matches!(func.as_ref(), Expr::Name(python_ast::NameExpr {id, ..}) if id == "reveal_type")
            )
        }).expect("to find `reveal_type` function call");
        let expr_to_infer = reveal_type_func
            .node()
            .as_call_expr()
            .unwrap()
            .arguments
            .find_positional(0)
            .expect("expression to type infer");

        let (scope_id, _) = db.find_enclosing_scope(&path, expr_to_infer.range().start().to_u32());
        let mut type_inferer = TypeInferer::new(&db, scope_id, path);
        let resolved_type = type_inferer.infer_expr(expr_to_infer, stack.nodes());

        assert_eq!(resolved_type.display(&db), expected)
    }

    #[test]
    fn test_infer_class_type() {
        let src = r#"
class Hello:
    def __init__(self): ...

reveal_type(Hello())
"#;
        assert_type(src, "test.py", "class[Hello]")
    }

    #[test]
    fn test_infer_list_type() {
        let src = r#"
reveal_type([1, 2, 3, 4, "hello", 3.14])
"#;
        assert_type(src, "test.py", "class[list]")
    }

    #[test]
    fn test_infer_tuple_type() {
        let src = r#"
reveal_type((1, 2, "hello", 3.14))
"#;
        assert_type(src, "test.py", "class[tuple]")
    }

    #[test]
    fn test_infer_dict_type() {
        let src = r#"
reveal_type({1: 2, 3: 4})
"#;
        assert_type(src, "test.py", "class[dict]")
    }

    #[test]
    fn test_infer_list_comp_type() {
        let src = r#"
l = [1, 2, 3]
reveal_type([i for i in l])
"#;
        assert_type(src, "test.py", "class[list]")
    }

    #[test]
    fn test_infer_bin_op_type() {
        let src = r#"
reveal_type([1,2,3] + ["hello"])
"#;
        assert_type(src, "test.py", "class[list]")
    }

    #[test]
    fn test_infer_module_import() {
        let src = r#"
from foo import bar 
reveal_type(bar.A())
"#;
        assert_type(src, "../resources/tests/fixtures/foobar.py", "class[A]")
    }

    #[test]
    fn test_infer_class_self_param_type() {
        let src = r#"
class Foo: ...
class Test:
    def __init__(self):
        self.a = 1
        reveal_type(self)
"#;
        assert_type(src, "test.py", "class[Test]")
    }

    #[test]
    fn test_infer_builtin_symbol() {
        let src = r#"
reveal_type(int)
"#;
        assert_type(src, "test.py", "class[int]")
    }

    #[test]
    fn test_infer_attribute_access() {
        let src = r#"
class Foo:
    def __init__(self): ...

    def sum(self, a, b) -> int:
        return a + b

f = Foo()
reveal_type(f.sum(1, 2))
"#;
        assert_type(src, "./test.py", "class[int]")
    }

    #[test]
    fn test_infer_function() {
        let src = r#"
def foo(): ...

reveal_type(foo)
"#;
        assert_type(src, "./test.py", "function")
    }

    #[test]
    fn test_infer_annotations() {
        let src = r#"
import typing

reveal_type(typing.List[typing.Union[int, str]])
"#;
        assert_type(src, "./test.py", "class[list]")
    }

    #[test]
    fn test_infer_super_class_method() {
        let src = r#"
class Super2:
    def __init__(self): ...
    def do_thing(self) -> int: ...

class Super1:
    def __init__(self): ...
    def do_thing(self) -> bool: ...

class Base(Super1, Super2):
    def __init__(self):
        self.a = 1
        reveal_type(self.do_thing())
"#;
        assert_type(src, "test.py", "class[bool]")
    }

    #[test]
    fn test_infer_match_pattern_bind() {
        let src = r#"
class Point:
    def __init__(self):
        self.x = 0
        self.y = 0
point = Point()
match point:
     case x: reveal_type(x)
"#;
        assert_type(src, "test.py", "class[Point]")
    }
}
