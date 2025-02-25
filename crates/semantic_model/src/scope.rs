use std::path::PathBuf;

use compact_str::CompactString;
use ruff_index::{newtype_index, IndexVec};
use ruff_text_size::TextRange;
use rustc_hash::FxHashMap;

use crate::{db::SymbolTableDb, symbol::SymbolId};

#[newtype_index]
pub struct ScopeId;

impl ScopeId {
    pub fn global() -> Self {
        ScopeId::from_u32(0)
    }

    pub fn sentinel() -> Self {
        ScopeId::MAX
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScopeKind {
    Module,
    Class,
    Function,
    Lambda,
    Comprehension,
}

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
    kind: ScopeKind,
    symbols: FxHashMap<CompactString, SymbolId>,
    /// NOTE: An empty range with `0..0` is a special case for the global scope.
    range: TextRange,
}

impl Scope {
    pub fn new(
        kind: ScopeKind,
        parent: Option<ScopeId>,
        symbols: FxHashMap<CompactString, SymbolId>,
        range: TextRange,
    ) -> Self {
        Self {
            parent,
            kind,
            symbols,
            range,
        }
    }

    pub fn global() -> Self {
        Self {
            kind: ScopeKind::Module,
            parent: None,
            symbols: FxHashMap::default(),
            range: TextRange::empty(0.into()),
        }
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    pub fn parent_scopes<'db, 'path>(
        &'db self,
        db: &'db SymbolTableDb,
        path: &'path PathBuf,
    ) -> impl Iterator<Item = &'db Scope> + use<'db, 'path> {
        std::iter::successors(Some(self), |&scope| {
            scope.parent().map(|scope_id| db.scope(path, scope_id))
        })
        .skip(1)
    }

    pub fn symbol_id(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    pub fn symbol_name(&self, symbol_id: SymbolId) -> Option<&str> {
        self.symbols
            .iter()
            .find(|(_, &id)| symbol_id == id)
            .map(|(name, _)| name.as_str())
    }

    pub fn symbol_names(&self) -> impl Iterator<Item = &CompactString> {
        self.symbols.keys()
    }

    pub fn symbols(&self) -> impl Iterator<Item = (&CompactString, &SymbolId)> {
        self.symbols.iter()
    }

    pub fn has_symbols(&self) -> bool {
        !self.symbols.is_empty()
    }

    pub fn add_symbol(&mut self, name: &str, symbol_id: SymbolId) {
        self.symbols.insert(CompactString::new(name), symbol_id);
    }

    pub fn contains_symbol(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn range(&self) -> &TextRange {
        &self.range
    }

    pub fn kind(&self) -> ScopeKind {
        self.kind
    }
}

#[derive(Debug, Clone)]
pub struct Scopes(IndexVec<ScopeId, Scope>);

impl Scopes {
    pub fn insert(&mut self, scope: Scope) -> ScopeId {
        self.0.push(scope)
    }

    pub fn global(&self) -> &Scope {
        &self.0[ScopeId::global()]
    }

    pub fn global_mut(&mut self) -> &mut Scope {
        &mut self.0[ScopeId::global()]
    }

    pub fn get(&self, id: ScopeId) -> Option<&Scope> {
        self.0.get(id)
    }

    pub fn get_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
        self.0.get_mut(id)
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Scope> {
        self.0.iter()
    }

    pub fn reversed_scopes(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.0
            .iter()
            .enumerate()
            .rev()
            .map(|(i, scope)| (ScopeId::from_usize(i), scope))
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &Scope> + '_ {
        std::iter::successors(Some(&self.0[scope_id]), |&scope| {
            scope.parent.map(|scope_id| &self.0[scope_id])
        })
    }
}

impl Default for Scopes {
    fn default() -> Self {
        Self(IndexVec::from_raw(vec![Scope::global()]))
    }
}
