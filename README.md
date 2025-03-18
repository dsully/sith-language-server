# SithLSP

> [!WARNING]
> THIS SOFTWARE IS IN ALPHA STATE EXPECT BUGS, BREAKING CHANGES, CRASHES, EXPLOSIONS AND EVERYTHING IN BETWEEN!

An experimental language server for the Python programming language.

## Features

- 🪲 Syntax checking
- ↪ Go to definition
- 🔍 Find references
- 🖊️ Autocompletion
- 📝 Element renaming
- 🗨️ Hover to view details on variables, functions, and more (_only shows documentation for now_)
- 💅 Code formatting and linting powered by [Ruff](https://github.com/astral-sh/ruff)
- 💡 Highlighting of symbol references
- 🐍 Automatically detect the Python interpreter being used by your project
- ... and many more!

<details>
<summary>LSP methods implemented so far</summary>

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
- textDocument/hover
- textDocument/documentSymbol
- textDocument/codeAction
- textDocument/signatureHelp
- workspace/executeCommand
- workspace/didChangeWorkspaceFolders
- completionItem/resolve
- codeAction/resolve

</details>

## How to use

You can install SithLSP from the main branch with

```sh
$ cargo install --git https://github.com/LaBatata101/sith-language-server
```

To install a specific release use `--tag`

```sh
$ cargo install --git https://github.com/LaBatata101/sith-language-server --tag v0.2.3-alpha
```

You can also download the latest version of SithLSP from the [releases](https://github.com/LaBatata101/sith-language-server/releases) or
build it manually following the steps below. The VSCode extension can also be downloaded from the [releases](https://github.com/LaBatata101/sith-language-server/releases) page, it's the `.vsix` file.

### Building the project

Download the repository with

```sh
$ git clone https://github.com/LaBatata101/sith-language-server
```

now build the SithLSP binary (you'll need the latest version of the Rust compiler).

```sh
$ cd sith-language-server
$ cargo build --release
```

The binary will be located in the `target/release` folder. You should place the binary in `$PATH` or,
if you're using VSCode, you can use the `sith.executable` setting option.

### Building the VSCode extension

It's probably better to disable the `Python` or `Pylance` extensions, from Microsoft, to avoid any conflicts when
using this extension.

You can build the VSCode extension manually with

```sh
# First we need to build the extension package
$ npm install -g @vscode/vsce
$ cd sith-language-server/editors/vscode
# Install dependencies
$ npm install
# Build the extension package
$ npm run compile
$ vsce package
# Now manually install the extension
$ code --install-extension sith-language-server-*.vsix
```

### Neovim Configuration

If you're using [`nvim-lspconfig`](https://github.com/neovim/nvim-lspconfig) add this to your config

```lua
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")

if not configs.sith_lsp then
  local root_files = {
    "pyproject.toml",
    "requirements.txt",
    "Pipfile",
    "pyrightconfig.json",
    ".git",
  }
  configs.sith_lsp = {
    default_config = {
      cmd = { "/path/to/sith-lsp" },
      root_dir = function(fname)
        return lspconfig.util.root_pattern(unpack(root_files))(fname)
      end,
      single_file_support = true,
      filetypes = { "python" },
      settings = {
          -- Settings for the server goes here.
          -- Config example
          ruff = {
              lint = {
                  enable = true
              }
          }
      },
    },
  }
end
```

Otherwise, add this to your `init.lua` config file.

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
          -- Settings for the server goes here.
          -- Config example
          ruff = {
            lint = {
              enable = true
            }
          }
        },
      },
    })
  end,
})
```

### Helix Configuration

Add this to your `languages.toml` config file.

```toml
[language-server.sith-lsp]
command = "/path/to/sith-lsp"

# Config example
[language-server.sith-lsp.config.settings.ruff.lint]
enable = true

[[language]]
name = "python"
language-servers = [
  "sith-lsp",
]

```

# Settings

You can ommit the `sith` prefix from the settings if you are not on **VSCode**.

### sith.executable

**NOTE:** This setting is only valid in the VSCode extension.

Path to the language server executable.

- type: `string`
- default: `sith-lsp`

### sith-language-server.trace.server

**NOTE:** This setting is only valid in the VSCode extension.

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
If this setting is set `sith` won't search automatically for a Python interpreter.

- type: `string`
- default: `null`

### sith.ruff.enable

Whether to enable Ruff.

- type: `boolean`
- default: `true` if `sith.ruff.path` is set, `false` otherwise.

### sith.ruff.path

Path to the `ruff` executable, e.g., `[\"/path/to/ruff\"]`.

- type: `string`
- default: `null`

### sith.ruff.format.enable

Whether to enable Ruff formatting.

- type: `boolean`
- default: `true` if `sith.ruff.path` is set, `false` otherwise.

### sith.ruff.format.args

Additional command-line arguments to pass to `ruff format`, e.g., `\"args\": [\"--config=/path/to/pyproject.toml\"]`. Supports a subset of Ruff's command-line arguments, ignoring those that are required to operate the LSP, like `--force-exclude` and `--verbose`.

- type: `string[]`
- default: `[]`

### sith.ruff.lint.enable

Whether to enable Ruff linting.

- type: `boolean`
- default: `true` if `sith.ruff.path` is set, `false` otherwise.

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
