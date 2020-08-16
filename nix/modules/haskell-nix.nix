let
  haskell-nix-src = import ../source/haskell-nix.nix;
in
import haskell-nix-src {}
