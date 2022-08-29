{
  nixConfig.bash-prompt = "[nix-develop-fri-proofs:] ";
  description = "FRI-based ZKPs in Haskell";
  inputs = {
    # Nixpkgs set to specific URL for haskellNix
    nixpkgs.url = "github:NixOS/nixpkgs/baaf9459d6105c243239289e1e82e3cdd5ac4809";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs-2205.url = "github:NixOS/nixpkgs/22.05";

    #CI integration
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
    sydtest-src = {
      url = "github:NorfairKing/sydtest/314d53ae175b540817a24d4211dab24fe6cb9232";
      flake = false;
    };
    validity-src = {
      url = "github:NorfairKing/validity/f5e5d69b3502cdd9243b412c31ba9619b9e89462";
      flake = false;
    };

    #HaskellNix is implemented using a set nixpkgs.follows; allowing for flake-build
    haskellNix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:input-output-hk/haskell.nix";
    };

    lint-utils = {
      url = "git+https://gitlab.homotopic.tech/nix/lint-utils";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs-2205";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-2205, flake-utils, lint-utils, sydtest-src, validity-src, haskellNix, flake-compat, flake-compat-ci }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (import "${sydtest-src}/nix/overlay.nix")
          (import "${validity-src}/nix/overlay.nix")
          (final: prev: {
            fri-proofs =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc922";
                projectFileName = "stack.yaml";
                modules = [{
                  packages = { };
                }];
                shell.buildInputs = with pkgs; [
                  cabal-install
                  ghcid
                  # tools below are used by lint-utils. Since lint-utils follows
                  # nixpkgs-2205, it only makes sense that tools available on
                  # the shell are using binaries from the same pkgs
                  # (22.05). This is a consequence of haskellNix not updating
                  # its nixpkgs-unstable.
                  pkgs-2205.haskell-language-server
                  pkgs-2205.nixpkgs-fmt
                  pkgs-2205.stylish-haskell
                  pkgs-2205.hlint
                  pkgs-2205.haskellPackages.cabal-fmt
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
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
       pkgs-2205 = import nixpkgs-2205 { inherit system overlays; };
        flake = pkgs.fri-proofs.flake { };
      in
      flake // {

        ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
          flake = self;
          systems = [ "x86_64-linux" ];
        };
        defaultPackage = flake.packages."fri-proofs:lib:fri-proofs";
        checks = flake.checks // {
          cabal-fmt = lint-utils.outputs.linters.${system}.cabal-fmt ./.;
          hlint = lint-utils.outputs.linters.${system}.hlint ./.;
          nixpkgs-fmt = lint-utils.outputs.linters.${system}.nixpkgs-fmt ./.;
          stylish-haskell = lint-utils.outputs.linters.${system}.stylish-haskell ./.;
        };
      });
}
