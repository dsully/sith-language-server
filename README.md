# SithLSP

> [!WARNING]
> THIS SOFTWARE IS IN ALPHA STATE EXPECT BUGS, BREAKING CHANGES, CRASHES, EXPLOSIONS AND EVERYTHING IN BETWEEN!

An experimental Python language server.

# LSP methods implemented so far

- textDocument/completion
- textDocument/publishDiagnostics
- textDocument/definition
- textDocument/references
- textDocument/formatting
- textDocument/didChange
- textDocument/documentHighlight
- textDocument/didOpen
- textDocument/didClose
- textDocument/rename
- textDocument/prepareRename
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

Add this to your `init.lua` config file.

```lua
local pwd = vim.loop.cwd()
vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  callback = function()
    vim.lsp.start({
      name = "SithLSP",
      filetypes = { "python" },
      root_dir = pwd,
      cmd = { "/path/to/sith-lsp" },
      init_options = {
        settings = {
          -- This setting is required for SithLSP to work properly.
          -- Not settings this will result in limited functionality.
          interpreter = "/path/to/python"
        },
      },
    })
  end,
})
```

# Settings

### sith.executable

Path to the language server executable.

- type: `string`
- default: `sith-lsp`

### sith-language-server.trace.server

Traces the communication between VS Code and the language server.

- type: `string`
- default: `off`
- options: `off`, `messages`, `verbose`

### sith.logLevel

Controls the log level of the language server.

- type: `string`
- default: `null`
- options: `error`, `warning`, `info`, `debug`, `trace`

### sith.logFile

Path to the log file for the language server.

- type: `string`
- default: `null`

### sith.interpreter

Path to a Python interpreter to use to run the LSP server.

- type: `string`
- default: `null`

### sith.ruff.path

Path to the `ruff` executable, e.g., `[\"/path/to/ruff\"]`.

- type: `string`
- default: `null`

### sith.ruff.format.enable

Whether to enable Ruff formatting.

- type: `boolean`
- default: `true`

### sith.ruff.format.args

Additional command-line arguments to pass to `ruff format`, e.g., `\"args\": [\"--config=/path/to/pyproject.toml\"]`. Supports a subset of Ruff's command-line arguments, ignoring those that are required to operate the LSP, like `--force-exclude` and `--verbose`.

- type: `string[]`
- default: `[]`

### sith.ruff.lint.enable

Whether to enable Ruff linting.

- type: `boolean`
- default: `true`

### sith.ruff.lint.args

Additional command-line arguments to pass to `ruff check`, e.g., `\"args\": [\"--config=/path/to/pyproject.toml\"]`. Supports a subset of Ruff's command-line arguments, ignoring those that are required to operate the LSP, like `--force-exclude` and `--verbose`.

- type: `string[]`
- default: `[]`

### sith.ruff.lint.select

Set rule codes to enable. Use `ALL` to enable all rules. See [the documentation](https://docs.astral.sh/ruff/settings/#lint_select) for more details.

- type: `string[]`
- default: `null`

### sith.ruff.lint.extendSelect

Enable additional rule codes on top of existing configuration, instead of overriding it. Use `ALL` to enable all rules.

- type: `string[]`
- default: `null`

### sith.ruff.lint.ignore

Set rule codes to disable. See [the documentation](https://docs.astral.sh/ruff/settings/#lint_ignore) for more details.

- type: `string[]`
- default: `null`

# Acknowledgements

- [Ruff](https://github.com/astral-sh/ruff) - ~~stole~~ borrowed lots of code from them.
