use bitflags::bitflags;
use ruff_index::{newtype_index, IndexVec};
use ruff_text_size::TextRange;

use crate::{
    declaration::{DeclId, SymbolDeclarations},
    ScopeId,
};

#[newtype_index]
pub struct SymbolId;

impl SymbolId {
    pub fn sentinel() -> Self {
        SymbolId::MAX
    }
}

#[derive(Debug, Default, Clone)]
pub struct Symbols(IndexVec<SymbolId, Symbol>);

impl Symbols {
    pub fn insert(&mut self, symbol: Symbol) -> SymbolId {
        self.0.push(symbol)
    }
    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.0.get(id)
    }

    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
        self.0.get_mut(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolOccurrence {
    Declaration(TextRange),
    Reference(TextRange),
}

impl SymbolOccurrence {
    pub fn is_declaration(&self) -> bool {
        matches!(self, SymbolOccurrence::Declaration(_))
    }

    pub fn range(&self) -> TextRange {
        match self {
            SymbolOccurrence::Declaration(range) | SymbolOccurrence::Reference(range) => *range,
        }
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct SymbolFlags: u8 {
        const PRIVATE = 1 << 0;
        const BUILTIN = 1 << 1;
        const CONSTANT = 1 << 2;
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    declaration_ids: SymbolDeclarations,
    /// Scope where the symbol was defined.
    definition_scope_id: ScopeId,
    references: Vec<SymbolOccurrence>,
    flags: SymbolFlags,
}

impl Symbol {
    pub fn new(scope_id: ScopeId, declaration_ids: SymbolDeclarations) -> Self {
        Self {
            declaration_ids,
            definition_scope_id: scope_id,
            references: Vec::new(),
            flags: SymbolFlags::empty(),
        }
    }

    pub fn is_builtin(&self) -> bool {
        self.flags.contains(SymbolFlags::BUILTIN)
    }

    pub fn is_private(&self) -> bool {
        self.flags.contains(SymbolFlags::PRIVATE)
    }

    pub fn is_constant(&self) -> bool {
        self.flags.contains(SymbolFlags::CONSTANT)
    }

    pub fn set_declaration_id(&mut self, decl_id: SymbolDeclarations) {
        self.declaration_ids = decl_id;
    }

    pub fn set_flag(&mut self, flag: SymbolFlags) {
        self.flags.insert(flag);
    }

    pub fn push_reference(&mut self, kind: SymbolOccurrence) {
        self.references.push(kind);
    }

    pub fn references(&self) -> &Vec<SymbolOccurrence> {
        &self.references
    }

    pub fn definition_scope(&self) -> ScopeId {
        self.definition_scope_id
    }

    pub fn declarations(&self) -> &SymbolDeclarations {
        &self.declaration_ids
    }

    pub fn push_declaration_id(&mut self, decl_id: DeclId) {
        self.declaration_ids.push(decl_id)
    }
}
