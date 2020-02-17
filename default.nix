let
  pkgs = import <nixpkgs> {};
  source = pkgs.lib.sourceByRegex ./. [
    "^init\.el$"
    "^mk.*$"
    "^test-startup\.py$"
  ];
in
pkgs.stdenv.mkDerivation {
  name = "dot-emacs";
  src = source;
  buildInputs = [
    pkgs.emacs26
    pkgs.python3
  ];
  buildPhase = ''
    python test-startup.py
  '';
  installPhase = ''
    mkdir "$out"
    cp mk/*.elc $out
  '';
}
