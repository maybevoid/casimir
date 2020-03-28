{ nixpkgs ? import ./nixpkgs.nix {} }:
(import ./release/8.8.3.nix { inherit nixpkgs; }).shell
