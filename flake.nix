{
  description = "haskell-fri-proofs";
  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    lint-utils = {
      url = "git+https://gitlab.homotopic.tech/nix/lint-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    horizon-orbis = {
      url = "git+ssh://git@github.com/Orbis-Tertius/horizon-orbis";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  outputs =
    inputs@
    { self
    , flake-utils
    , horizon-orbis
    , lint-utils
    , nixpkgs
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      hsPkgs =
        with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc942.override
          {
            overrides = hfinal: hprev:
              horizon-orbis.packages.x86_64-linux //
              {
                fri-proofs = disableLibraryProfiling (hprev.callCabal2nix "fri-proofs" ./. { });
              };
          };
    in
    {
      devShells.default = hsPkgs.fri-proofs.env.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [
          hsPkgs.cabal-install
          pkgs.nixpkgs-fmt
          pkgs.ghcid
        ];
      });
      packages.default = hsPkgs.fri-proofs;
      checks =
        {
          hlint = lint-utils.outputs.linters.${system}.hlint self;
          hpack = lint-utils.outputs.linters.${system}.hpack self;
          nixpkgs-fmt = lint-utils.outputs.linters.${system}.nixpkgs-fmt self;
          stylish-haskell = lint-utils.outputs.linters.${system}.stylish-haskell self;
        };
    });
}
