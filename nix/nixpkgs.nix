nixOptions :
let
  nixpkgsSource = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "300af061809fc0d93fd486060b4a15c90b23a8be";
    ref = "haskell-updates";
  };
in
import nixpkgsSource nixOptions
