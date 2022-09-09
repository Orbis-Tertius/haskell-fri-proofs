{
  nixConfig.bash-prompt = "[nix-develop-fri-proofs:] ";
  description = "FRI-based ZKPs in Haskell";
  inputs = {
    # Nixpkgs set to specific URL for haskellNix
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    #CI integration
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    #HaskellNix is implemented using a set nixpkgs.follows; allowing for flake-build
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix,  flake-compat, flake-compat-ci }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            fri-proofs =
              final.haskell-nix.cabalProject' {
                src = ./.;
                compiler-nix-name = "ghc924";
                shell.tools = {
                  cabal = { };
                  ghcid = { };
                  hlint = { };
                  haskell-language-server = { };
                  stylish-haskell = { };
                  sydtest-discover = { };
                };
                shell.exactDeps = true;
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                  cabal-install
                ];
                shell.shellHook =
                  ''
                  manual-ci() (
                    set -e

                    ./ci/lint.sh
                    cabal test
                    nix-build
                  )
                  '';
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.fri-proofs.flake { };
      in flake // {
        
        ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
          flake = self;
          systems = [ "x86_64-linux" ];
        };
        defaultPackage = flake.packages."fri-proofs:lib:fri-proofs";
      });
}
