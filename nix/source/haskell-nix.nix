let
  commit = "cd429f6ea6946c7a2fc78fc55a1b11cfd73acc86";
  sha256 = "sha256:1n1br8hq2bka6z8zdprbcd64a8rpd2xcyp26rh5vykxqih0wcy3v";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/input-output-hk/haskell.nix/archive/${commit}.tar.gz";
  }
