let
  commit = "00a9d3f261d7da2891801abe35728ab93794644a";
  sha256 = "sha256:0im0d4rspdla352rn2ls17sakzfr27mliw3jyixmwwgi6i3jwd78";
in
builtins.fetchTarball
  { inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
  }
