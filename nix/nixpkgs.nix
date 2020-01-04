nixOptions :
let
  nixpkgsSource = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "f1568223c9cd39f6771a58944fb8f79ac03f1e85";
  };
in
import nixpkgsSource nixOptions
