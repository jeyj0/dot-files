{ sources ? import ./sources.nix }:
let
  overlay = _: pkgs: {
    niv = (import sources.niv {}).niv;

    responsively = pkgs.callPackage (import ./pkgs/responsively.nix) {};
  };
in
  import sources.nixpkgs {
    overlays = [
      overlay
    ];
    config = {
      allowUnfree = true;
    };
  }
