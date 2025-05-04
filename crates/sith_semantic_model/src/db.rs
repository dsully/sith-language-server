use std::path::PathBuf;

use sith_python_utils::PythonHost;

use crate::{
    declaration::{DeclId, Declaration, DeclarationQuery},
    indexer::{BuiltinSymbolTable, Indexer},
    symbol::{Symbol, SymbolId},
    symbol_table::SymbolTable,
    Scope, ScopeId,
};

pub enum Source<'a> {
    New(&'a str),
    Update { old: &'a str, new: &'a str },
}

#[derive(Debug, Clone)]
pub struct SymbolTableDb {
    index: Indexer,
}

impl SymbolTableDb {
    pub fn new(root: PathBuf, python_host: PythonHost) -> Self {
        Self {
            index: Indexer::new(root, python_host)
                .with_builtin_symbols()
                .with_collection_types()
                .index(),
        }
    }

    pub fn indexer(&self) -> &Indexer {
        &self.index
    }

    pub fn indexer_mut(&mut self) -> &mut Indexer {
        &mut self.index
    }

    pub fn builtin_symbols(&self) -> &BuiltinSymbolTable {
        self.indexer().builtin_symbol_table()
    }

    pub fn collection_stub_symbol_table(&self) -> &SymbolTable {
        self.table(self.collection_stub_path())
    }

    pub fn collection_stub_path(&self) -> &PathBuf {
        self.indexer().collection_stub_path()
    }

    pub fn table(&self, file: &PathBuf) -> &SymbolTable {
        match self.indexer().table(&self.indexer().file_id(file)) {
            Some(table) => table,
            None if file.ends_with("stdlib/builtins.pyi") => self.builtin_symbols().symbol_table(),
            _ => panic!("no symbol table for path: {}", file.display()),
        }
    }

    pub fn symbol(&self, file: &PathBuf, symbol_id: SymbolId) -> &Symbol {
        self.table(file)
            .symbol(symbol_id)
            .or_else(|| self.builtin_symbols().symbol(symbol_id))
            .expect("symbol for id")
    }

    pub fn lookup_symbol(
        &self,
        file: &PathBuf,
        name: &str,
        scope: ScopeId,
    ) -> Option<(SymbolId, &Symbol)> {
        self.table(file).lookup_symbol(name, scope).or_else(|| {
            self.builtin_symbols()
                .lookup_symbol(name, ScopeId::global())
        })
    }

    pub fn symbol_name(&self, file: &PathBuf, symbol_id: SymbolId) -> &str {
        let symbol = self.symbol(file, symbol_id);
        let scope = self.scope(file, symbol.definition_scope());
        scope.symbol_name(symbol_id).unwrap()
    }

    pub fn declaration(&self, file: &PathBuf, decl_id: DeclId) -> &Declaration {
        self.table(file)
            .declaration(decl_id)
            .expect("expect declaration for id")
    }

    pub fn symbol_declaration(
        &self,
        file: &PathBuf,
        name: &str,
        scope_id: ScopeId,
        query: DeclarationQuery,
    ) -> Option<&Declaration> {
        self.table(file)
            .symbol_declaration(name, scope_id, query)
            .or_else(|| self.builtin_symbols().symbol_declaration(name))
    }

    pub fn find_enclosing_scope(&self, file: &PathBuf, offset: u32) -> (ScopeId, &Scope) {
        self.table(file).find_enclosing_scope(offset)
    }

    /// Returns the global [`Scope`] of a `file`.
    pub fn global_scope(&self, file: &PathBuf) -> &Scope {
        self.table(file).scope(ScopeId::global()).unwrap()
    }

    pub fn scope(&self, file: &PathBuf, scope_id: ScopeId) -> &Scope {
        self.table(file).scope(scope_id).expect("scope to exist")
    }

    pub fn scopes(&self, file: &PathBuf) -> impl Iterator<Item = &Scope> {
        self.table(file).scopes.iter()
    }
}
