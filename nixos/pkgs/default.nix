let
  nixpkgs = import <nixpkgs> {};
  callPackage = nixpkgs.callPackage;
in
{
  responsively = callPackage ./responsively.nix {};
  dat-desktop = callPackage ./dat-desktop.nix {};
  spare-actre = callPackage ./spare-actre.nix {};
}

