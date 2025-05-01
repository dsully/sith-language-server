use std::fmt::Display;

use super::{
    lsp_client::{LspClientMockup, TestRequestHandler},
    SymbolLocationPrinter,
};
use crate::{
    server::api::requests::{self as req, goto_definition::goto_definition},
    PositionEncoding,
};
use lsp_types::{self as types};

impl TestRequestHandler for req::GotoDefinition {
    fn build_params(
        uri: lsp_types::Url,
        position: lsp_types::Position,
    ) -> types::GotoDefinitionParams {
        types::GotoDefinitionParams {
            text_document_position_params: types::TextDocumentPositionParams {
                text_document: types::TextDocumentIdentifier { uri },
                position,
            },
            work_done_progress_params: types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
        }
    }
}

struct GotoDefinitionOutput {
    result: Option<types::GotoDefinitionResponse>,
    encoding: PositionEncoding,
}

impl GotoDefinitionOutput {
    fn new(result: Option<types::GotoDefinitionResponse>, encoding: PositionEncoding) -> Self {
        Self { result, encoding }
    }
}

impl Display for GotoDefinitionOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(response) = self.result.as_ref() else {
            return writeln!(f, "No definition found!");
        };

        match response {
            types::GotoDefinitionResponse::Scalar(location) => {
                SymbolLocationPrinter::new(location, self.encoding).fmt(f)?;
            }
            _ => unimplemented!("{:?}", response),
        }

        Ok(())
    }
}

#[test]
fn goto_definition_same_file() {
    let src = r#"
def foo(): ...

def test():
  fo<cursor>o()
"#;
    let client = LspClientMockup::new_project("goto-definition-same-file").add_file("foo.py", src);

    client.request::<req::GotoDefinition>("foo.py", src, |params, snapshot| {
        let encoding = snapshot.encoding();
        insta::assert_snapshot!(GotoDefinitionOutput::new(
            goto_definition(snapshot, params),
            encoding
        ));
    });
}

#[test]
fn goto_definition_builtin_typeshed() {
    let src = r#"
i<cursor>nt
"#;
    let client =
        LspClientMockup::new_project("goto-definition-builtin-typeshed").add_file("foo.py", src);

    client.request::<req::GotoDefinition>("foo.py", src, |params, snapshot| {
        let encoding = snapshot.encoding();
        insta::assert_snapshot!(GotoDefinitionOutput::new(
            goto_definition(snapshot, params),
            encoding
        ));
    });
}

#[test]
fn goto_definition_across_files() {
    let src1 = r#"
def external_func(): ...
"#;
    let src2 = r#"
from file1 import external_func

external_<cursor>func()
"#;
    let client = LspClientMockup::new_project("goto-definition-across-files")
        .add_file("file1.py", src1)
        .add_file("file2.py", src2);

    client.request::<req::GotoDefinition>("file2.py", src2, |params, snapshot| {
        let encoding = snapshot.encoding();
        insta::assert_snapshot!(GotoDefinitionOutput::new(
            goto_definition(snapshot, params),
            encoding
        ));
    });
}

#[test]
fn goto_class_attribute_definition() {
    let src1 = r#"
class Foo:
  def __init__(self):
    self.attr = None
"#;
    let src2 = r#"
from file1 import Foo

f = Foo()
print(f.at<cursor>tr)
"#;
    let client = LspClientMockup::new_project("goto-class-attribute-definition")
        .add_file("file1.py", src1)
        .add_file("file2.py", src2);

    client.request::<req::GotoDefinition>("file2.py", src2, |params, snapshot| {
        let encoding = snapshot.encoding();
        insta::assert_snapshot!(GotoDefinitionOutput::new(
            goto_definition(snapshot, params),
            encoding
        ));
    });
}

#[test]
fn goto_module_attribute_definition() {
    let src1 = r#"
def thingy(): ...
"#;
    let src2 = r#"
import file1

print(file1.thin<cursor>gy)
"#;
    let client = LspClientMockup::new_project("goto-module-attribute-definition")
        .add_file("file1.py", src1)
        .add_file("file2.py", src2);

    client.request::<req::GotoDefinition>("file2.py", src2, |params, snapshot| {
        let encoding = snapshot.encoding();
        insta::assert_snapshot!(GotoDefinitionOutput::new(
            goto_definition(snapshot, params),
            encoding
        ));
    });
}
