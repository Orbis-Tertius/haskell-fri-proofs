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
                fri-proofs-spec = disableLibraryProfiling (hprev.callCabal2nix "fri-proofs:spec" ./. { });
              };
          };
      ormolu-check =
        pkgs.stdenv.mkDerivation {
          name = "ormolu-check";
          src = ./.;
          buildPhase = ''
            ${pkgs.git.outPath}/bin/git init
            ${pkgs.git.outPath}/bin/git add -A
            ${pkgs.git.outPath}/bin/git config user.email "foo@bar.com"
            ${pkgs.git.outPath}/bin/git config user.name "Foobar"
            ${pkgs.git.outPath}/bin/git commit -m "initial commit"
            ${pkgs.ormolu.outPath}/bin/ormolu -m inplace $(find ./. -type f -name '*.hs')
            if [ -z "$(${pkgs.git.outPath}/bin/git status --porcelain)" ]; then
              echo "ok"
            else
              echo "ormolu check failed"
              exit 1
            fi
          '';
          installPhase = ''
            mkdir -p $out
          '';
        };
    in
    {
      devShells.default = hsPkgs.fri-proofs.env.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [
          hsPkgs.cabal-install
          pkgs.nixpkgs-fmt
          hsPkgs.ghcid
          pkgs.ormolu
          hsPkgs.hlint
        ];
      });
      packages.default = hsPkgs.fri-proofs;
      packages.ormolu-check = ormolu-check;
      checks =
        {
          hlint = lint-utils.outputs.linters.${system}.hlint self;
          hpack = lint-utils.outputs.linters.${system}.hpack self;
          nixpkgs-fmt = lint-utils.outputs.linters.${system}.nixpkgs-fmt self;
          inherit ormolu-check;
          spec = hsPkgs.fri-proofs-spec;
        };
    });
}
