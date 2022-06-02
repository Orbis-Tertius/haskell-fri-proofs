{
  nixConfig.bash-prompt = "[nix-develop-stark-anatomy:] ";
  description = "Hash-based STARK implementation in Haskell for pedagogical purposes";
  inputs = {
    # Nixpkgs set to specific URL for haskellNix
    nixpkgs.url = "github:NixOS/nixpkgs/baaf9459d6105c243239289e1e82e3cdd5ac4809";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

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
  };

  outputs = { self, nixpkgs, flake-utils, sydtest-src, validity-src, haskellNix,  flake-compat, flake-compat-ci }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (import "${sydtest-src}/nix/overlay.nix")
          (import "${validity-src}/nix/overlay.nix")
          (final: prev: {
            stark-anatomy =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                projectFileName = "cabal.project";
                modules = [{
                  packages = {
                  };
                }];
                shell.tools = {
                  cabal = { };
                  ghcid = { };
                  hlint = { };
                  haskell-language-server = { };
                  stylish-haskell = { };
                  sydtest-discover = { };
                };
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
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
        flake = pkgs.stark-anatomy.flake { };
      in flake // {
        
        ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
          flake = self;
          systems = [ "x86_64-linux" ];
        };
        defaultPackage = flake.packages."stark-anatomy:lib:stark-anatomy";
      });
}
