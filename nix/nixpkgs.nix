nixOptions :
let
  nixpkgsSource = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "448bed5653ec36db5959f423abf5a0529337c760";
  };
in
import nixpkgsSource nixOptions
