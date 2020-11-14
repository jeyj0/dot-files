{ sources ? import ./sources.nix }:
let
  overlay = _: pkgs: {
    niv = (import sources.niv {}).niv;

    responsively = pkgs.callPackage (import ./pkgs/responsively.nix) {};
    nnn = pkgs.callPackage (import ./pkgs/nnn.nix) {};
    battery-status = import ../../scripts/battery-status;
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
