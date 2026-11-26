{
  description = "agdalicious: an Agda language server and neovim plugin with an infoview and unicode input";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages;

        agda = pkgs.agda.withPackages (p: [ p.standard-library ]);

        agdalicious-lsp = hpkgs.callCabal2nix "agdalicious-lsp" ./server { };

        agdalicious-nvim = pkgs.vimUtils.buildVimPlugin {
          pname = "agdalicious-nvim";
          version = "0.1.0";
          src = ./nvim;
        };

        # A throwaway neovim with the plugin, the server and agda on PATH,
        # for trying things out: `nix run .#nvim -- test/Example.agda`
        nvim-dev = pkgs.writeShellApplication {
          name = "nvim";
          runtimeInputs = [ agdalicious-lsp agda ];
          text = ''
            exec ${pkgs.neovim}/bin/nvim -u NONE \
              --cmd 'set rtp^=${agdalicious-nvim}' \
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
          inherit agdalicious-lsp agdalicious-nvim;
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
          ];
          shellHook = ''
            echo "agdalicious dev shell"
            echo "  build server:  (cd server && cabal build)"
            echo "  try plugin:    nix run .#nvim -- test/Example.agda"
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
