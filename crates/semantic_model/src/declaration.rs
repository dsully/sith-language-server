use std::{
    ops::{Index, IndexMut},
    path::PathBuf,
};

use python_ast_utils::nodes::NodeId;
use ruff_index::{newtype_index, IndexVec};
use ruff_text_size::TextRange;

use crate::{symbol::SymbolId, symbol_table::SymbolTable, ScopeId};

#[newtype_index]
pub struct DeclId;

#[derive(Debug, Default, Clone)]
pub struct Declarations(IndexVec<DeclId, Declaration>);

impl Declarations {
    pub fn insert(&mut self, kind: DeclarationKind, node_id: NodeId, range: TextRange) -> DeclId {
        self.0.push(Declaration {
            symbol_id: 0u32.into(),
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
    Import { source: Option<PathBuf> },
    ImportSegment { source: Option<PathBuf> },
    ImportAlias(DeclId),
    AnnAssign,
    AugAssign,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub symbol_id: SymbolId,
    pub node_id: NodeId,
    pub kind: DeclarationKind,
    pub range: TextRange,
}

impl Declaration {
    pub fn import_source(&self) -> Option<&PathBuf> {
        let DeclarationKind::Stmt(DeclStmt::Import { source } | DeclStmt::ImportSegment { source }) =
            &self.kind
        else {
            return None;
        };
        source.as_ref()
    }

    pub fn body_scope(&self) -> Option<ScopeId> {
        match self.kind {
            DeclarationKind::Stmt(
                DeclStmt::Function(id) | DeclStmt::Method(_, id) | DeclStmt::Class(id),
            ) => Some(id),
            _ => None,
        }
    }
}

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
}
