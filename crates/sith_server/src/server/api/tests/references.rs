use std::fmt::Display;

use lsp_types::{self as types, request::Request, Location, Url};
use ruff_source_file::{LineIndex, OneIndexed};

use crate::{
    edit::RangeExt,
    server::api::{
        requests::{self as req, references::references},
        tests::strip_temp_dir,
    },
    PositionEncoding,
};

use super::lsp_client::{LspClientMockup, TestRequestHandler};

impl TestRequestHandler for req::References {
    fn build_params(
        uri: Url,
        position: lsp_types::Position,
    ) -> <Self::RequestType as Request>::Params {
        types::ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
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
            let file_path = location.uri.to_file_path().unwrap();
            let stripped_path = strip_temp_dir(&file_path);

            let contents = std::fs::read_to_string(&file_path).expect("Failed to read file!");
            let index = LineIndex::from_source_text(&contents);

            if location.range.start.line != location.range.end.line {
                return writeln!(
                    f,
                    "ERROR: A symbol reference can not be across multiple lines.\nStart line: {}\nEnd line: {}",
                    location.range.start.line, location.range.end.line
                );
            }

            let line_idx = location.range.start.line;
            let line_str = contents.lines().nth(line_idx as usize).unwrap();
            let line_start_offset =
                index.line_start(OneIndexed::from_zero_indexed(line_idx as usize), &contents);
            let range = location
                .range
                .to_text_range(&contents, &index, self.encoding);
            let symbol = &contents[range];
            // Normalize the range to be within the line range.
            let start_symbol_idx = range
                .sub_start(line_start_offset)
                .sub_end(line_start_offset)
                .start()
                .to_usize();

            writeln!(
                f,
                "{}\n{}",
                stripped_path.display(),
                "-".repeat(stripped_path.as_os_str().len())
            )?;
            writeln!(
                f,
                "{line_str}   LINE: {} ({}..{})\n{}{}\n\n",
                line_idx + 1,
                location.range.start.character + 1,
                location.range.end.character + 1,
                " ".repeat(start_symbol_idx),
                "^".repeat(symbol.len())
            )?;
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
