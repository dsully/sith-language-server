use std::{
    ops::{Index, IndexMut},
    path::PathBuf,
    sync::Arc,
};

use sith_python_ast_utils::nodes::NodeId;
use sith_python_utils::is_python_module;
use ruff_index::{newtype_index, IndexVec};
use ruff_text_size::TextRange;
use serde::{Deserialize, Serialize};

use crate::{db::FileId, symbol::SymbolId, symbol_table::SymbolTable, ScopeId};

#[newtype_index]
#[derive(Serialize, Deserialize)]
pub struct DeclId;

#[derive(Debug, Default, Clone)]
pub struct Declarations(IndexVec<DeclId, Declaration>);

impl Declarations {
    pub fn insert(
        &mut self,
        file_id: FileId,
        kind: DeclarationKind,
        node_id: NodeId,
        range: TextRange,
    ) -> DeclId {
        self.0.push(Declaration {
            file_id,
            symbol_id: SymbolId::sentinel(),
            node_id,
            kind,
            range,
        })
    }

    pub fn get(&self, index: DeclId) -> Option<&Declaration> {
        self.0.get(index)
    }

    pub fn get_mut(&mut self, index: DeclId) -> Option<&mut Declaration> {
        self.0.get_mut(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Declaration> {
        self.0.iter()
    }
}

impl Index<DeclId> for Declarations {
    type Output = Declaration;

    fn index(&self, index: DeclId) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<DeclId> for Declarations {
    fn index_mut(&mut self, index: DeclId) -> &mut Self::Output {
        &mut self.0[index]
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationKind {
    Stmt(DeclStmt),
    Parameter,
    InstanceParameter(SymbolId),
    For,
    WithItem,
    MatchAs,
    Exception,
    Named,

    TypeVar,
    TypeParamSpec,
    TypeVarTuple,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeclStmt {
    Function(ScopeId),
    Method(SymbolId, ScopeId),
    Class(ScopeId),
    TypeAlias,
    Assignment,
    Import(ImportSource),
    ImportStar(ImportSource),
    ImportSegment(ImportSource),
    ImportAlias(DeclId),
    /// Special-case: this used when we have the same import name multiple times in the source
    /// code. Example:
    /// ```
    /// from foo.bar import Bar
    /// from foo import Foo
    /// ```
    /// In this case symbol `foo` is used twice, this would create two different [`DeclStmt::Import`] declarations,
    /// to save some memory we just point to the first `foo` declaration made.
    ///
    /// This is only used for computing the LSP diagnostics of a document, any other LSP operation
    /// will ignore this.
    SameImport(DeclId),
    AnnAssign,
    AugAssign,
}

#[derive(Debug, Default, Clone, Eq)]
pub struct ImportSource {
    pub(crate) stub: Option<Arc<PathBuf>>,
    pub(crate) non_stub: Option<Arc<PathBuf>>,
    pub(crate) is_thirdparty: bool,
}

impl ImportSource {
    pub fn stub(path: PathBuf) -> Self {
        Self {
            stub: Some(Arc::new(path)),
            non_stub: None,
            is_thirdparty: false,
        }
    }

    pub fn non_stub(path: PathBuf) -> Self {
        Self {
            stub: None,
            non_stub: Some(Arc::new(path)),
            is_thirdparty: false,
        }
    }

    /// Returns the path to the source file for import-related declarations,
    /// with preference given to stub sources over non-stub sources when available.
    ///
    /// Due to the way indexing works, this function always returns the indexed
    /// path.
    pub fn any_path(&self) -> Option<&Arc<PathBuf>> {
        self.stub.as_ref().or(self.non_stub.as_ref())
    }

    pub fn stub_path(&self) -> Option<&Arc<PathBuf>> {
        self.stub.as_ref()
    }

    pub fn non_stub_path(&self) -> Option<&Arc<PathBuf>> {
        self.non_stub.as_ref()
    }

    pub fn is_unresolved(&self) -> bool {
        self.stub.is_none() && self.non_stub.is_none()
    }

    pub fn is_thirdparty(&self) -> bool {
        self.is_thirdparty
    }
}

impl From<&PathBuf> for ImportSource {
    fn from(path: &PathBuf) -> Self {
        let ext = path.extension().expect("File to have extension!");
        if ext == "pyi" {
            Self::stub(path.to_path_buf())
        } else {
            Self::non_stub(path.to_path_buf())
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub file_id: FileId,
    pub symbol_id: SymbolId,
    pub node_id: NodeId,
    pub kind: DeclarationKind,
    pub range: TextRange,
}

impl Declaration {
    pub fn is_import(&self) -> bool {
        matches!(
            self.kind,
            DeclarationKind::Stmt(
                DeclStmt::Import { .. }
                    | DeclStmt::ImportAlias(_)
                    | DeclStmt::ImportSegment { .. }
                    | DeclStmt::ImportStar { .. }
            )
        )
    }

    pub fn is_instance_parameter(&self) -> bool {
        matches!(self.kind, DeclarationKind::InstanceParameter(_))
    }

    pub fn is_parameter(&self) -> bool {
        matches!(self.kind, DeclarationKind::InstanceParameter(_))
    }

    pub fn is_method(&self) -> bool {
        matches!(self.kind, DeclarationKind::Stmt(DeclStmt::Method(_, _)))
    }

    pub fn import_source(&self) -> Option<&ImportSource> {
        match &self.kind {
            DeclarationKind::Stmt(
                DeclStmt::Import(import_source)
                | DeclStmt::ImportSegment(import_source)
                | DeclStmt::ImportStar(import_source),
            ) => Some(import_source),
            _ => None,
        }
    }

    pub fn body_scope(&self) -> Option<ScopeId> {
        match self.kind {
            DeclarationKind::Stmt(
                DeclStmt::Function(id) | DeclStmt::Method(_, id) | DeclStmt::Class(id),
            ) => Some(id),
            _ => None,
        }
    }

    pub fn is_imported_module(&self, symbol_name: &str) -> bool {
        match &self.kind {
            DeclarationKind::Stmt(
                DeclStmt::Import(import_source)
                | DeclStmt::ImportSegment(import_source)
                | DeclStmt::ImportStar(import_source),
            ) if import_source.stub.is_some() => {
                is_python_module(symbol_name, import_source.stub.as_ref().unwrap())
            }
            DeclarationKind::Stmt(
                DeclStmt::Import(import_source)
                | DeclStmt::ImportSegment(import_source)
                | DeclStmt::ImportStar(import_source),
            ) if import_source.non_stub.is_some() => {
                is_python_module(symbol_name, import_source.non_stub.as_ref().unwrap())
            }
            _ => false,
        }
    }
}

impl PartialEq for ImportSource {
    fn eq(&self, other: &Self) -> bool {
        self.stub == other.stub || self.non_stub == other.non_stub
    }
}

#[derive(Clone, Copy)]
pub enum DeclarationQuery {
    /// Get the first declaration made.
    First,
    /// Get the last declaration made.
    Last,
    /// Get the declaration made immediately before the specified offset position.
    AtOffset(u32),
}

#[derive(Debug, Clone)]
pub struct SymbolDeclarations {
    inner: SymbolDeclarationsInner,
}

#[derive(Debug, Clone)]
enum SymbolDeclarationsInner {
    Single(DeclId),
    Multiple(Vec<DeclId>),
}

impl SymbolDeclarations {
    pub fn single(decl_id: DeclId) -> Self {
        Self {
            inner: SymbolDeclarationsInner::Single(decl_id),
        }
    }

    pub fn first(&self) -> DeclId {
        match &self.inner {
            SymbolDeclarationsInner::Single(id) => *id,
            SymbolDeclarationsInner::Multiple(ids) => ids.first().copied().unwrap(),
        }
    }

    pub fn last(&self) -> DeclId {
        match &self.inner {
            SymbolDeclarationsInner::Single(id) => *id,
            SymbolDeclarationsInner::Multiple(ids) => ids.last().copied().unwrap(),
        }
    }

    pub fn at_offset<'a>(&self, table: &'a SymbolTable, offset: u32) -> Option<&'a Declaration> {
        match &self.inner {
            SymbolDeclarationsInner::Single(id) => table.decls.get(*id),
            SymbolDeclarationsInner::Multiple(ids) => ids
                .iter()
                .rev()
                .map(|id| table.decls.get(*id).unwrap())
                .find(|declaration| offset >= declaration.range.start().to_u32()),
        }
    }

    pub fn push(&mut self, id: DeclId) {
        match &mut self.inner {
            SymbolDeclarationsInner::Single(previous_id) => {
                self.inner = SymbolDeclarationsInner::Multiple(vec![*previous_id, id]);
            }
            SymbolDeclarationsInner::Multiple(ids) => ids.push(id),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &DeclId> {
        match &self.inner {
            SymbolDeclarationsInner::Single(decl_id) => std::slice::from_ref(decl_id).iter(),
            SymbolDeclarationsInner::Multiple(decl_ids) => decl_ids.iter(),
        }
    }
}
