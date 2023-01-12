{
  inputs = {
    haskellNix.url = github:input-output-hk/haskell.nix;
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            lhx =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc925";
                shell = {
                  exactDeps = true;
                  nativeBuildInputs = [ pkgs.nodejs-16_x ];
                  tools = {
                    cabal = {};
                    fourmolu = {};
                    haskell-language-server = {};
                  };
                };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.lhx.flake {};
      in flake // {
        legacyPackages = pkgs;
      });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    # allow-import-from-derivation = "true";
  };
}
