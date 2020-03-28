nixOptions :
let
  nixpkgsSource = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "e75b0af2b68ff82df4ce2827039e0d2361b4abc9";
    ref = "haskell-updates";
  };
in
import nixpkgsSource nixOptions
