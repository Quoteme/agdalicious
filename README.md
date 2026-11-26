# agdalicious

Agda support for neovim, modelled on lean.nvim:

- `server/`: **agdalicious-lsp**, a Haskell language server. It does the actual work: loading files in Agda, goals, diagnostics.
- `nvim/`: a thin Lua plugin. It starts the server, shows the infoview (`:AgdaInfo`) and expands `\`-abbreviations as you type (`\lambda` → `λ`, `\to` → `→`, `\bN` → `ℕ`, ...).
- `tree-sitter-agda/`: a tree-sitter grammar for highlighting before (or without) Agda. It parses declarations and layout exactly and expressions as flat sequences, since mixfix operators can't be parsed without scope information. All 1266 files of agda-stdlib 2.4 parse without errors. Build it with `nix build .#agdalicious-treesitter` (a runtimepath directory with `parser/agda.so` and `queries/agda/`). The plugin starts it on `.agda` buffers when it is on the runtimepath. Develop with `cd tree-sitter-agda && tree-sitter generate && tree-sitter test`.
- `docs/agda-interaction.md`: what `agda --interaction-json` and the Emacs mode can do: every command, response and key binding.

Both are skeletons for now. The infoview request (`agda/infoview`) returns placeholder text.

## Development

```sh
nix develop                              # ghc, cabal, hls, agda + stdlib, neovim, lua-ls
(cd server && cabal build)
nix build .#agdalicious-lsp               # the server
nix build .#agdalicious-nvim              # the plugin, as a vim plugin derivation
nix run .#nvim -- test/Example.agda      # throwaway neovim with both wired up
```

## Using the plugin

```lua
require("agdalicious").setup({
  lsp = { cmd = { "agdalicious-lsp" } },
  unicode = { extra = { ["bZ"] = "ℤ" } },
})
```

`:AgdaInfo [open|close|toggle]` opens the infoview, which follows the cursor.

### Unicode input

An abbreviation is replaced as soon as it can't be extended any further (`\lambda`). An ambiguous one, such as `\to` (which could still become `\top`), is replaced when the next character rules out the longer ones, or when you leave insert mode. The table is in `nvim/lua/agdalicious/abbreviations.lua`.

## Protocol

`agda/infoview` (client → server request)

- params: `{ textDocument: { uri }, position: { line, character } }`
- result: `{ goals: [{ name, goalType, context: [string] }], messages: [string] }`
