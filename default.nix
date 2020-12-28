{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
nixpkgs.haskell.lib.doBenchmark ((nixpkgs.pkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    # src = ../cabal-toolkit;
  };
}).callPackage ./datafix.nix { })
