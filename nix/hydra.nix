let
  jobs = {
    build = { system ? builtins.currentSystem }:
      let
        nixpkgs = import ./nixpkgs.nix { inherit system; };
      in

      import ./release.nix {
        inherit nixpkgs;
      };
  };
in
  jobs
