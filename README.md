# agdalicious

Agda support for neovim, modelled on lean.nvim:

- `server/`: **agdalicious-lsp**, a Haskell language server. It does the actual work: loading files in Agda, goals, diagnostics.
- `nvim/`: a thin Lua plugin. It starts the server, shows the infoview (`:AgdaInfo`) and expands `\`-abbreviations as you type (`\lambda` → `λ`, `\to` → `→`, `\bN` → `ℕ`, ...).

Every open and edit type-checks the buffer (unsaved contents included) in a persistent `agda --interaction-json` process, along with the other open files that may import it, and shows errors and warnings as diagnostics. The infoview request (`agda/infoview`) still returns placeholder text.

## Development

```sh
nix develop                              # ghc, cabal, hls, agda + stdlib, neovim, lua-ls
(cd server && cabal build)
nix build .#agdalicious-lsp               # the server
nix build .#agdalicious-nvim              # the plugin, as a vim plugin derivation
nix run .#nvim -- test/Example.agda      # throwaway neovim with both wired up
```

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

## Protocol

`agda/infoview` (client → server request)

- params: `{ textDocument: { uri }, position: { line, character } }`
- result: `{ goals: [{ name, goalType, context: [string] }], messages: [string] }`
