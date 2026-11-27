# agdalicious

Agda support for neovim, modelled on lean.nvim:

- `server/`: **agdalicious-lsp**, a Haskell language server. It does the actual work: loading files in Agda, goals, diagnostics.
- `nvim/`: a thin Lua plugin. It starts the server, shows the infoview (`:AgdaInfo`), expands `\`-abbreviations as you type (`\lambda` → `λ`, `\to` → `→`, `\bN` → `ℕ`, ...) and makes `<C-a>`/`<C-x>` count in sub- and superscripts.
- `tree-sitter-agda/`: a tree-sitter grammar for highlighting before (or without) Agda. It parses declarations and layout exactly and expressions as flat sequences, since mixfix operators can't be parsed without scope information. All 1266 files of agda-stdlib 2.4 parse without errors. Build it with `nix build .#agdalicious-treesitter` (a runtimepath directory with `parser/agda.so` and `queries/agda/`). The plugin starts it on `.agda` buffers when it is on the runtimepath. Develop with `cd tree-sitter-agda && tree-sitter generate && tree-sitter test`.
- `docs/agda-interaction.md`: what `agda --interaction-json` and the Emacs mode can do: every command, response and key binding.

Every open and edit type-checks the buffer (unsaved contents included) in a persistent `agda --interaction-json` process, along with the other open files that may import it, and shows errors and warnings as diagnostics. Go to definition (`vim.lsp.buf.definition()`, `gd` with most configs) uses the definition sites Agda reports while checking, hovering (`K`) explains what is under the cursor (see [Hover](#hover)), and completion offers keywords, pragmas, options, modules and names (see [Completion](#completion)). The infoview request (`agda/infoview`) still returns placeholder text.

## Development

```sh
nix develop                              # ghc, cabal, hls, agda + stdlib, neovim, lua-ls
(cd server && cabal build)
nix build .#agdalicious-lsp               # the server
nix build .#agdalicious-nvim              # the plugin, as a vim plugin derivation
nix run .#nvim -- test/Example.agda      # throwaway neovim with both wired up
nix build .#agdalicious-manual            # the hover texts from the Agda user manual
```

The dev shell links the generated hover texts to `server/data/manual.json`, where `cabal build` compiles them in. Without that file the server still builds, just without them.

## Using the plugin

With [lazy.nvim](https://github.com/folke/lazy.nvim):

```lua
{
  "Quoteme/agdalicious",
  ft = "agda",
  config = function(plugin, opts)
    -- the neovim plugin lives in the repository's nvim/ directory
    vim.opt.rtp:append(plugin.dir .. "/nvim")
    vim.cmd.runtime("plugin/agdalicious.lua")
    require("agdalicious").setup(opts)
  end,
  opts = {
    lsp = { cmd = { "agdalicious-lsp" } },
    unicode = { extra = { ["bZ"] = "ℤ" } },
  },
}
```

Or call `setup` yourself:

```lua
require("agdalicious").setup({
  lsp = { cmd = { "agdalicious-lsp" } },
  unicode = { extra = { ["bZ"] = "ℤ" } },
})
```

`:AgdaInfo [open|close|toggle]` opens the infoview, which follows the cursor.

### Unicode input

An abbreviation is replaced as soon as it can't be extended any further (`\lambda`). An ambiguous one, such as `\to` (which could still become `\top`), is replaced when the next character rules out the longer ones, or when you leave insert mode. The table is in `nvim/lua/agdalicious/abbreviations.lua`.

### Sub- and superscripts

In Agda buffers `<C-a>` and `<C-x>` also increment and decrement numbers written in sub- or superscript digits: `x₁` → `x₂`, `n⁹` → `n¹⁰`, `x₀` → `x₋₁`. They pick the number under or after the cursor, as the built-in commands do, and take a count and repeat with `.`. Plain numbers are left to the built-in commands. Turn this off with `increment = { enabled = false }`.

## Hover

Hovering shows, as far as they exist: the type, a short explanation, the definition (if short) or an example, links to the documentation, and a long explanation.

- **Names**, from any library: the type comes from Agda. The rest is read from the source of the definition when you hover, following the standard library's conventions: the comment just above a declaration explains it, a comment under a line of dashes is the heading of the part of the file that follows, and the comment at the top of a file explains the module. Libraries with HTML documentation (configured in `scripts/doc-sources.json`; the standard library is) get a link to the definition there, others to the file.
- **Keywords, symbols, pragmas, options, built-ins and primitives** (`data`, `with`, `OPTIONS`, `--safe`, `Set`, `ℕ` from `Agda.Builtin.Nat`, ...): from the [Agda user manual](https://agda.readthedocs.io/), bundled with the server.
- **Holes**: the goal and what is in scope there.

`scripts/gen-manual.py` generates the bundled part at build time from the manual's sources (the `agda-src` flake input, or the release tarball in CI). Which keywords, pragmas, built-ins and options there are, and where the manual explains them, comes from the manual itself; `scripts/doc-sources.json` has the URLs and the one keyword (`public`) whose section can't be found automatically.

## Completion

What is offered depends on where the cursor is:

| Where | Offered |
|---|---|
| `{-# ` | pragmas (`OPTIONS`, `BUILTIN`, `TERMINATING`, ...) |
| `{-# OPTIONS --gu` | the options a pragma can set (`--guarded`, `--guardedness`, ...) |
| `{-# BUILTIN ` | built-ins (`NATURAL`, `EQUALITY`, ...) |
| `open import Agda.Bui` | the modules the file can import: its project's, its libraries' (from its `.agda-lib` and Agda's libraries file) and Agda's built-in ones |
| `open import Agda.Builtin.IO using (` (also `hiding`, `renaming`, `open N using (` for `import M as N`, and lists over several lines) | what the module exports, with types |
| `N.su`, `Data.Nat.su` | the names an `import` makes available qualified (by the module, or the name after `as`) |
| elsewhere | keywords, the names the file's `open import`s and `open`s of imported modules bring into scope (respecting `using`, `hiding` and `renaming`) and the names it defines |

Keywords, pragmas, options and built-ins come with the manual's explanation; a module shows the comment at the top of its file once selected. What a module exports comes from Agda, which checks a scratch module importing it; the first time that can take a few seconds (Agda loads the module's dependencies), after that it is cached.

## Protocol

`agda/infoview` (client → server request)

- params: `{ textDocument: { uri }, position: { line, character } }`
- result: `{ goals: [{ name, goalType, context: [string] }], messages: [string] }`
