{
  description = "agdalicious: an Agda language server and neovim plugin with an infoview and unicode input";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # the sources of the Agda user manual, for the hover texts; keep the
    # version in step with nixpkgs' agda
    agda-src = {
      url = "github:agda/agda/v2.8.0";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, agda-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages;

        agda = pkgs.agda.withPackages (p: [ p.standard-library ]);

        # Hover texts for keywords, pragmas, options and built-ins, generated
        # from the user manual and compiled into the server.
        agdalicious-manual = pkgs.runCommand "agdalicious-manual.json" { nativeBuildInputs = [ pkgs.python3 ]; } ''
          python3 ${./scripts/gen-manual.py} ${agda-src}/doc/user-manual ${./scripts/doc-sources.json} > $out
        '';

        agdalicious-lsp = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "agdalicious-lsp" ./server { }) (old: {
          preBuild = (old.preBuild or "") + ''
            # the commit for --version (the source in the store has no .git)
            export AGDALICIOUS_COMMIT=${self.rev or self.dirtyRev or "unknown"}
            mkdir -p data && cp ${agdalicious-manual} data/manual.json
          '';
        });

        agdalicious-nvim = pkgs.vimUtils.buildVimPlugin {
          pname = "agdalicious-nvim";
          version = "0.1.0";
          src = ./nvim;
        };

        # The tree-sitter grammar (tree-sitter-agda/), compiled.
        tree-sitter-agda = pkgs.tree-sitter.buildGrammar {
          language = "agda";
          version = "0.1.0";
          src = ./tree-sitter-agda;
        };

        # The grammar laid out as a runtimepath directory, the way neovim
        # looks for parsers and queries: parser/agda.so, queries/agda/*.scm.
        agdalicious-treesitter = pkgs.runCommand "agdalicious-treesitter" { } ''
          mkdir -p $out/parser $out/queries
          ln -s ${tree-sitter-agda}/parser $out/parser/agda.so
          ln -s ${tree-sitter-agda}/queries $out/queries/agda
        '';

        # A throwaway neovim with the plugin, the server and agda on PATH,
        # for trying things out: `nix run .#nvim -- test/Example.agda`
        nvim-dev = pkgs.writeShellApplication {
          name = "nvim";
          runtimeInputs = [ agdalicious-lsp agda ];
          text = ''
            exec ${pkgs.neovim}/bin/nvim -u NONE \
              --cmd 'set rtp^=${agdalicious-nvim}' \
              --cmd 'set rtp^=${agdalicious-treesitter}' \
              --cmd 'filetype plugin on' \
              --cmd 'syntax on' \
              -c 'lua require("agdalicious").setup()' \
              "$@"
          '';
        };
      in
      {
        packages = {
          default = agdalicious-lsp;
          inherit agdalicious-lsp agdalicious-nvim agdalicious-manual tree-sitter-agda agdalicious-treesitter;
          nvim = nvim-dev;
        };

        apps.nvim = flake-utils.lib.mkApp { drv = nvim-dev; };

        devShells.default = hpkgs.shellFor {
          packages = _: [ agdalicious-lsp ];
          nativeBuildInputs = [
            # Haskell tooling
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.fourmolu
            # Agda itself (the server drives it)
            agda
            # Neovim + Lua tooling for the plugin
            pkgs.neovim
            pkgs.lua-language-server
            pkgs.stylua
            # Grammar development: (cd tree-sitter-agda && tree-sitter generate && tree-sitter test)
            pkgs.tree-sitter
            pkgs.nodejs
          ];
          shellHook = ''
            # the generated hover texts, for `cabal build` (see scripts/gen-manual.py)
            if [ -d server ]; then mkdir -p server/data && ln -sfn ${agdalicious-manual} server/data/manual.json; fi
            echo "agdalicious dev shell"
            echo "  build server:  (cd server && cabal build)"
            echo "  try plugin:    nix run .#nvim -- test/Example.agda"
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
