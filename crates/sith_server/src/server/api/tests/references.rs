use std::fmt::Display;

use lsp_types::{self as types, Location, Url};

use crate::{
    server::api::requests::{self as req, references::references},
    PositionEncoding,
};

use super::{
    lsp_client::{LspClientMockup, TestRequestHandler},
    SymbolLocationPrinter,
};

impl TestRequestHandler for req::References {
    fn build_params(uri: Url, position: lsp_types::Position) -> types::ReferenceParams {
        types::ReferenceParams {
            text_document_position: types::TextDocumentPositionParams {
                text_document: types::TextDocumentIdentifier { uri },
                position,
            },
            context: types::ReferenceContext {
                include_declaration: true,
            },
            work_done_progress_params: types::WorkDoneProgressParams::default(),
            partial_result_params: lsp_types::PartialResultParams::default(),
        }
    }
}

struct ReferencesOutput {
    results: Option<Vec<Location>>,
    encoding: PositionEncoding,
}

impl ReferencesOutput {
    fn new(results: Option<Vec<Location>>, encoding: PositionEncoding) -> Self {
        Self { results, encoding }
    }
}

impl Display for ReferencesOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.results.is_none()
            || self
                .results
                .as_ref()
                .is_some_and(|result| result.is_empty())
        {
            return writeln!(f, "No references found!");
        }

        for location in self.results.as_ref().unwrap() {
            SymbolLocationPrinter::new(location, self.encoding)
                .underline_whole_symbol()
                .fmt(f)?;
        }

        Ok(())
    }
}

#[test]
fn references_same_file() {
    let src = r#"
def foo(): ...

def test():
  fo<cursor>o()
"#;
    let client = LspClientMockup::new_project("references-same-file").add_file("foo.py", src);

    client.request::<req::References>("foo.py", src, |params, snapshot| {
        insta::assert_snapshot!(ReferencesOutput::new(
            references(&snapshot, params),
            snapshot.encoding()
        ));
    });
}

#[test]
fn references_across_files() {
    let src1 = r#"
class TimeCategory(object):
    pass
"#;
    let src2 = r#"
from models import TimeCategory


def get_hash():
    pass


def view(param, items):
    ref = Time<cursor>Category.SOON
    return (param or []) + [get_hash(p) for p in items or []]
"#;

    let client = LspClientMockup::new_project("test-references-across-files")
        .add_file("models.py", src1)
        .add_file("views.py", src2);

    client.request::<req::References>("views.py", src2, |params, snapshot| {
        insta::assert_snapshot!(ReferencesOutput::new(
            references(&snapshot, params),
            snapshot.encoding()
        ));
    });
}

#[test]
fn local_variable_reference() {
    let src = r#"
def process_data(items):
    count = 0
    for item in items:
        if item > 10:
            cou<cursor>nt += 1
    return count
"#;
    let client =
        LspClientMockup::new_project("test-local-var-references").add_file("processor.py", src);

    client.request::<req::References>("processor.py", src, |params, snapshot| {
        insta::assert_snapshot!(ReferencesOutput::new(
            references(&snapshot, params),
            snapshot.encoding()
        ));
    });
}

#[test]
fn class_attribute_reference() {
    let src1 = r#"
class User:
    def __init__(self, name):
        self.name = name

    def greet(self):
        print(f"Hello, {self.na<cursor>me}")

def run():
    user = User("Alice")
    print(user.name)
    user.greet()
"#;

    let src2 = r#"
from user_model import User

user = User("Bob")
print(user.name)
"#;
    let client = LspClientMockup::new_project("test-class-attr-references")
        .add_file("user_model.py", src1)
        .add_file("consumer.py", src2);

    client.request::<req::References>("user_model.py", src1, |params, snapshot| {
        insta::assert_snapshot!(ReferencesOutput::new(
            references(&snapshot, params),
            snapshot.encoding()
        ));
    });
}

#[test]
fn module_attribute_reference() {
    let src1 = r#"
def func1(): ...

print(func1())
"#;

    let src2 = r#"
import module1

module1.fun<cursor>c1()
"#;
    let src3 = r#"
import module1

module1.func1()
"#;
    let client = LspClientMockup::new_project("test-module-attr-references")
        .add_file("module1.py", src1)
        .add_file("module2.py", src2)
        .add_file("module3.py", src3);

    client.request::<req::References>("module2.py", src2, |params, snapshot| {
        insta::assert_snapshot!(ReferencesOutput::new(
            references(&snapshot, params),
            snapshot.encoding()
        ));
    });
}

#[test]
fn no_references_found() {
    let src = r#"
no_referen<cursor>ces
"#;
    let client = LspClientMockup::new_project("test-no-references").add_file("calculator.py", src);

    client.request::<req::References>("calculator.py", src, |params, snapshot| {
        insta::assert_snapshot!(ReferencesOutput::new(
            references(&snapshot, params),
            snapshot.encoding()
        ));
    });
}
