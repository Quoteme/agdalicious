# agdalicious

Agda support for neovim, modelled on lean.nvim:

- `server/`: **agdalicious-lsp**, a Haskell language server. It does the actual work: loading files in Agda, goals, diagnostics.
- `nvim/`: a thin Lua plugin. It starts the server, shows the infoview (`:AgdaInfo`), expands `\`-abbreviations as you type (`\lambda` → `λ`, `\to` → `→`, `\bN` → `ℕ`, ...) and makes `<C-a>`/`<C-x>` count in sub- and superscripts.
- `tree-sitter-agda/`: a tree-sitter grammar for highlighting before (or without) Agda. It parses declarations and layout exactly and expressions as flat sequences, since mixfix operators can't be parsed without scope information. All 1266 files of agda-stdlib 2.4 parse without errors. Build it with `nix build .#agdalicious-treesitter` (a runtimepath directory with `parser/agda.so` and `queries/agda/`). The plugin starts it on `.agda` buffers when it is on the runtimepath. Develop with `cd tree-sitter-agda && tree-sitter generate && tree-sitter test`.
- `docs/agda-interaction.md`: what `agda --interaction-json` and the Emacs mode can do: every command, response and key binding.

Every open and edit type-checks the buffer (unsaved contents included) in a persistent `agda --interaction-json` process, along with the other open files that may import it, and shows errors and warnings as diagnostics. Go to definition (`vim.lsp.buf.definition()`, `gd` with most configs) uses the definition sites Agda reports while checking, hovering (`K`) explains what is under the cursor (see [Hover](#hover)), and completion offers keywords, pragmas, options, modules and names (see [Completion](#completion)). The infoview request (`agda/infoview`) still returns placeholder text.

## Screenshots

<img width="491" height="480" alt="image" src="https://github.com/user-attachments/assets/c0145cca-3aad-4720-a022-bc5f0e0cc66d" />

<img width="502" height="460" alt="image" src="https://github.com/user-attachments/assets/5b72239a-9318-4beb-a99a-83ecdbff6d78" />

<img width="708" height="513" alt="image" src="https://github.com/user-attachments/assets/9cf14585-1ec6-469f-99c7-405ca93ef345" />

<img width="472" height="575" alt="image" src="https://github.com/user-attachments/assets/a1c9a354-1479-4834-8f51-d6ec6874a043" />

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

With [lazy.nvim](https://github.com/folke/lazy.nvim), from the `dist` branch:

```lua
{
  "Quoteme/agdalicious",
  branch = "dist",
  ft = "agda",
  opts = {
    lsp = { cmd = { "agdalicious-lsp" } },
    unicode = { extra = { ["bZ"] = "ℤ" } },
  },
}
```

Make sure you also have `agdalicious-lsp` on your `PATH`, for example by running:

```bash
nix shell github:Quoteme/agdalicious#agdalicious-lsp
```

If you also want haskell-language-server support inside `FOREIGN GHC ...` blocks, ensure `ghc` and `haskell-language-server` are also available (both from the same nixpkgs, see [Foreign code](#foreign-code)):

```bash
nix shell github:Quoteme/agdalicious#agdalicious-lsp nixpkgs#haskell-language-server nixpkgs#ghc
```

Manual installation can also be done by calling `setup` yourself:

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

## Code actions

For a name that is not in scope, the code actions (`vim.lsp.buf.code_action()`) offer what Agda suggests in its error (`did you mean 'Agda.Builtin.Nat._+_'?`): importing the name from a module the file imports already (added to the `using` list of its `open`, or as `open M using (x)` after a plain `import M`), or writing a suggestion instead (`suc` for `sucx`). When there is only one fix, it is marked preferred, so `vim.lsp.buf.code_action({ apply = true })` applies it at once. Agda only suggests names of modules that are imported: for others there is nothing to offer yet.

## Foreign code

The code in `FOREIGN` and `COMPILE` pragmas belongs to its backend's language:

```agda
{-# FOREIGN GHC import qualified Data.Text.IO as Text #-}
{-# COMPILE GHC putStrLn = Text.putStrLn #-}
```

tree-sitter highlights it with that language's parser (`queries/injections.scm`: `GHC` is Haskell, other backends are their name in lower case, such as `c` or `cpp`), if neovim has one. The server hands it to that language's server: each backend's code becomes a file of its own, next to the document in the server's mirror, and hovering, going to definitions and completing there are answered by that server, whose diagnostics show up next to Agda's. Of a `COMPILE` pragma, only the expression after `=` is Haskell; the Agda name before it keeps Agda's hover.

The servers are `haskell-language-server-wrapper --lsp` for `GHC`, `clangd` for `C` and `CPP` and `typescript-language-server --stdio` for `JS`. They are started when first needed, by `agdalicious-lsp` and not by neovim, so `:checkhealth vim.lsp` does not list them. They have to be on the `PATH` the server sees. With nix, for the server with haskell-language-server (and the GHC it needs, and Agda):

```sh
nix shell github:Quoteme/agdalicious#agdalicious-lsp nixpkgs#haskell-language-server nixpkgs#ghc nixpkgs#agda
```

(`.#agdalicious-lsp` in a checkout.) haskell-language-server only works with the very GHC it was built with: take both from the same nixpkgs, as here (a `ghc` of the same version from another nixpkgs does not do, the wrapper then stops with "GHC ABIs don't match"). If a server stops, the server shows why, with the end of its log. Other servers, or none, can be set in the plugin's options:

```lua
lsp = { foreign = { GHC = { command = { "haskell-language-server", "--lsp" } }, JS = vim.NIL } }
```

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
