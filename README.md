# SithLSP

> [!WARNING]
> THIS SOFTWARE IS A WORK IN PROGRESS!

An experimental Python language server.

# Implemented LSP methods so far

- textDocument/completion
- textDocument/publishDiagnostics
- textDocument/definition
- textDocument/references
- textDocument/didChange
- textDocument/didOpen
- textDocument/didClose
- workspace/didChangeWorkspaceFolders

# How to use

Download the repository with

```sh
$ git clone https://github.com/LaBatata101/sith-language-server
```

now build it (you'll need the latest version of the Rust compiler)

```sh
$ cd sith-language-server
$ cargo build --release
```

the binary will be located at `target/release`. You should put the binary in `$PATH` or
set the binary location in the SithLSP configuration option.

## Installing the VSCode extension

It's probably better to disable the `Python` or `Pylance` extensions, from Microsoft, to avoid any conflicts.

You can install the VSCode extension manually with

```sh
# First we need to build the extension package
$ npm install -g @vscode/vsce
$ cd sith-language-server/editors/vscode
$ vsce package
# Now manually install the extension
$ code --install-extension sith-language-server-0.1.0.vsix
```

## Neovim

TODO

# Settings

TODO
