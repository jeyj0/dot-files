let
  nixpkgs = import <nixpkgs> {};
  callPackage = nixpkgs.callPackage;
in
{
  responsively = callPackage ./responsively.nix {};
}

