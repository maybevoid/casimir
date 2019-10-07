{ nixpkgs, declInput }:
let
  pkgs = import nixpkgs {};
  jobs = ./jobs.json;
in
{
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cp ${jobs} $out
  '';
}
