use bitflags::bitflags;
use ruff_index::{newtype_index, IndexVec};

use crate::{
    declaration::{DeclId, SymbolDeclarations},
    ScopeId,
};

#[newtype_index]
#[derive(Ord, PartialOrd)]
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

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct SymbolFlags: u8 {
        const PRIVATE = 1 << 0;
        const BUILTIN = 1 << 1;
        const CONSTANT = 1 << 2;
        const CONTRUCTOR = 1 << 3;
        const CLASS_FIELD = 1 << 4;
        const MODULE = 1 << 5;
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    declaration_ids: SymbolDeclarations,
    /// Scope where the symbol was defined.
    definition_scope_id: ScopeId,
    flags: SymbolFlags,
}

impl Symbol {
    pub fn new(scope_id: ScopeId, declaration_ids: SymbolDeclarations) -> Self {
        Self {
            declaration_ids,
            definition_scope_id: scope_id,
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

    pub fn is_constructor(&self) -> bool {
        self.flags.contains(SymbolFlags::CONTRUCTOR)
    }

    pub fn is_class_field(&self) -> bool {
        self.flags.contains(SymbolFlags::CLASS_FIELD)
    }

    pub fn is_module(&self) -> bool {
        self.flags.contains(SymbolFlags::MODULE)
    }

    pub fn set_declaration_id(&mut self, decl_id: SymbolDeclarations) {
        self.declaration_ids = decl_id;
    }

    pub fn set_flag(&mut self, flag: SymbolFlags) {
        self.flags.insert(flag);
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
