let
pkgs = import <nixpkgs> {};
in
  [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
#    yarn
#    pkgs.nodejs-8_x
  ]
