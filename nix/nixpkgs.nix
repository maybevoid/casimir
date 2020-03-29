nixOptions :
let
  nixpkgsSource = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "eda60c51a9e6553631f01053bc05d9c914936249";
  };
in
import nixpkgsSource nixOptions
