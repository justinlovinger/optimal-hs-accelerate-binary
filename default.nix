{ pkgs ? import ./pkgs.nix { }, compiler ? "ghc8104" }:
pkgs.haskell.packages.${compiler}.callCabal2nix "optimal-binary-accelerate" ./. { }
