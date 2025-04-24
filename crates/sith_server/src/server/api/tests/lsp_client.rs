use std::path::{Path, PathBuf};

use lsp_types::{self as types, request::Request, Url};
use ruff_source_file::LineIndex;
use rustc_hash::FxHashMap;

use crate::{
    edit::DocumentVersion,
    server::api::RequestHandler,
    session::{ClientEditor, ClientSettings, DocumentSnapshot, Session},
    Server,
};

pub(super) trait TestRequestHandler: RequestHandler {
    fn build_params(
        uri: Url,
        position: types::Position,
    ) -> <<Self as RequestHandler>::RequestType as Request>::Params;
}

pub(super) struct LspClientMockup {
    root: PathBuf,
    session: Session,
    files: FxHashMap<PathBuf, PathBuf>,
}

impl LspClientMockup {
    pub(super) fn new_project(name: &str) -> Self {
        let client_capabilities = serde_json::from_str(NEOVIM_011_CAPABILITIES).unwrap();
        let root = std::env::temp_dir().join(name);
        if let Err(e) = std::fs::create_dir(&root) {
            panic!("Failed to create temp directory '{}'\n{e}", root.display());
        }

        Self {
            session: Session::new(
                &client_capabilities,
                Server::find_best_position_encoding(&client_capabilities),
                ClientSettings::default(),
                ClientEditor::Neovim,
                vec![(
                    Url::from_file_path(&root).unwrap(),
                    ClientSettings::default(),
                )],
            )
            .expect("Failed to create session!"),
            root,
            files: FxHashMap::default(),
        }
    }

    pub(super) fn add_file(mut self, file: impl AsRef<Path>, content: &str) -> Self {
        let file = file.as_ref();
        let content = content.replace("<cursor>", "");

        if let Some(parent) = file.parent().filter(|p| !p.as_os_str().is_empty()) {
            let parent_dir = self.root.join(parent);
            if !parent_dir.exists() {
                std::fs::create_dir_all(&parent_dir).expect("Failed to create dirs!");
            }
        }

        let file_path = self.root.join(file);
        std::fs::write(&file_path, &content)
            .unwrap_or_else(|e| panic!("Failed to create '{}' file!\n{e}", file.display()));

        self.session.open_document(
            &Url::from_file_path(&file_path).unwrap(),
            content,
            DocumentVersion::default(),
        );

        self.files.insert(file.to_path_buf(), file_path);

        self
    }

    pub(super) fn request<R: TestRequestHandler>(
        &self,
        file: &str,
        content: &str,
        handler: impl Fn(<R::RequestType as Request>::Params, DocumentSnapshot),
    ) {
        let uri = self.get_file_uri(file);
        let request_position = find_cursor_offset(content);

        handler(
            R::build_params(uri, request_position),
            self.document_snapshot(file),
        )
    }

    fn document_snapshot(&self, file: impl AsRef<Path>) -> DocumentSnapshot {
        let uri = self.get_file_uri(file);
        self.session
            .take_snapshot(&uri)
            .expect("Failed to take document snapshot!")
    }

    fn get_file_uri(&self, file: impl AsRef<Path>) -> Url {
        let file_path = self.files.get(file.as_ref()).unwrap();
        Url::from_file_path(file_path).unwrap()
    }
}

impl Drop for LspClientMockup {
    fn drop(&mut self) {
        if let Err(e) = std::fs::remove_dir_all(&self.root) {
            panic!(
                "Failed to remove temporary directory '{}'\n{e}",
                self.root.display()
            )
        }
    }
}

fn find_cursor_offset(content: &str) -> types::Position {
    let offset = u32::try_from(
        content
            .find("<cursor>")
            .expect("Missing <cursor> position in source."),
    )
    .expect("Failed to convert cursor offset to u32");
    let content = content.replace("<cursor>", "");

    let index = LineIndex::from_source_text(&content);
    let source_location = index.source_location(offset.into(), &content);

    types::Position {
        line: u32::try_from(source_location.row.to_zero_indexed())
            .expect("Failed to convert row offset to u32"),
        character: u32::try_from(source_location.column.to_zero_indexed())
            .expect("Failed to convert column offset to u32"),
    }
}

/// Neovim 0.11 LSP client capabilities.
pub(super) const NEOVIM_011_CAPABILITIES: &str = r#"{
  "general": {
    "positionEncodings": [
      "utf-8",
      "utf-16",
      "utf-32"
    ]
  },
  "textDocument": {
    "callHierarchy": {
      "dynamicRegistration": false
    },
    "codeAction": {
      "codeActionLiteralSupport": {
        "codeActionKind": {
          "valueSet": [
            "",
            "quickfix",
            "refactor",
            "refactor.extract",
            "refactor.inline",
            "refactor.rewrite",
            "source",
            "source.organizeImports"
          ]
        }
      },
      "dataSupport": true,
      "dynamicRegistration": true,
      "isPreferredSupport": true,
      "resolveSupport": {
        "properties": [
          "edit",
          "command"
        ]
      }
    },
    "codeLens": {
      "dynamicRegistration": false,
      "resolveSupport": {
        "properties": [
          "command"
        ]
      }
    },
    "completion": {
      "completionItem": {
        "commitCharactersSupport": false,
        "deprecatedSupport": true,
        "documentationFormat": [
          "markdown",
          "plaintext"
        ],
        "preselectSupport": false,
        "resolveSupport": {
          "properties": [
            "additionalTextEdits",
            "command"
          ]
        },
        "snippetSupport": true,
        "tagSupport": {
          "valueSet": [
            1
          ]
        }
      },
      "completionItemKind": {
        "valueSet": [
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10,
          11,
          12,
          13,
          14,
          15,
          16,
          17,
          18,
          19,
          20,
          21,
          22,
          23,
          24,
          25
        ]
      },
      "completionList": {
        "itemDefaults": [
          "editRange",
          "insertTextFormat",
          "insertTextMode",
          "data"
        ]
      },
      "contextSupport": true,
      "dynamicRegistration": false
    },
    "declaration": {
      "linkSupport": true
    },
    "definition": {
      "dynamicRegistration": true,
      "linkSupport": true
    },
    "diagnostic": {
      "dynamicRegistration": false
    },
    "documentHighlight": {
      "dynamicRegistration": false
    },
    "documentSymbol": {
      "dynamicRegistration": false,
      "hierarchicalDocumentSymbolSupport": true,
      "symbolKind": {
        "valueSet": [
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10,
          11,
          12,
          13,
          14,
          15,
          16,
          17,
          18,
          19,
          20,
          21,
          22,
          23,
          24,
          25,
          26
        ]
      }
    },
    "foldingRange": {
      "dynamicRegistration": false,
      "foldingRange": {
        "collapsedText": true
      },
      "lineFoldingOnly": true
    },
    "formatting": {
      "dynamicRegistration": true
    },
    "hover": {
      "contentFormat": [
        "markdown",
        "plaintext"
      ],
      "dynamicRegistration": true
    },
    "implementation": {
      "linkSupport": true
    },
    "inlayHint": {
      "dynamicRegistration": true,
      "resolveSupport": {
        "properties": [
          "textEdits",
          "tooltip",
          "location",
          "command"
        ]
      }
    },
    "publishDiagnostics": {
      "dataSupport": true,
      "relatedInformation": true,
      "tagSupport": {
        "valueSet": [
          1,
          2
        ]
      }
    },
    "rangeFormatting": {
      "dynamicRegistration": true,
      "rangesSupport": true
    },
    "references": {
      "dynamicRegistration": false
    },
    "rename": {
      "dynamicRegistration": true,
      "prepareSupport": true
    },
    "semanticTokens": {
      "augmentsSyntaxTokens": true,
      "dynamicRegistration": false,
      "formats": [
        "relative"
      ],
      "multilineTokenSupport": false,
      "overlappingTokenSupport": true,
      "requests": {
        "full": {
          "delta": true
        },
        "range": false
      },
      "serverCancelSupport": false,
      "tokenModifiers": [
        "declaration",
        "definition",
        "readonly",
        "static",
        "deprecated",
        "abstract",
        "async",
        "modification",
        "documentation",
        "defaultLibrary"
      ],
      "tokenTypes": [
        "namespace",
        "type",
        "class",
        "enum",
        "interface",
        "struct",
        "typeParameter",
        "parameter",
        "variable",
        "property",
        "enumMember",
        "event",
        "function",
        "method",
        "macro",
        "keyword",
        "modifier",
        "comment",
        "string",
        "number",
        "regexp",
        "operator",
        "decorator"
      ]
    },
    "signatureHelp": {
      "dynamicRegistration": false,
      "signatureInformation": {
        "activeParameterSupport": true,
        "documentationFormat": [
          "markdown",
          "plaintext"
        ],
        "parameterInformation": {
          "labelOffsetSupport": true
        }
      }
    },
    "synchronization": {
      "didSave": true,
      "dynamicRegistration": false,
      "willSave": true,
      "willSaveWaitUntil": true
    },
    "typeDefinition": {
      "linkSupport": true
    }
  },
  "window": {
    "showDocument": {
      "support": true
    },
    "showMessage": {
      "messageActionItem": {
        "additionalPropertiesSupport": true
      }
    },
    "workDoneProgress": true
  },
  "workspace": {
    "applyEdit": true,
    "configuration": true,
    "didChangeConfiguration": {
      "dynamicRegistration": false
    },
    "didChangeWatchedFiles": {
      "dynamicRegistration": false,
      "relativePatternSupport": true
    },
    "inlayHint": {
      "refreshSupport": true
    },
    "semanticTokens": {
      "refreshSupport": true
    },
    "symbol": {
      "dynamicRegistration": false,
      "symbolKind": {
        "valueSet": [
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10,
          11,
          12,
          13,
          14,
          15,
          16,
          17,
          18,
          19,
          20,
          21,
          22,
          23,
          24,
          25,
          26
        ]
      }
    },
    "workspaceEdit": {
      "resourceOperations": [
        "rename",
        "create",
        "delete"
      ]
    },
    "workspaceFolders": true
  }
}"#;
