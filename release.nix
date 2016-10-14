let
  pkgs = import <nixpkgs> ( {
  } );
  version = "0.2.1.0";
  lambda = pkgs.haskell.packages.ghc801;
in
rec {
  hascar =
    lambda.callPackage ./hascar.nix { };
}
