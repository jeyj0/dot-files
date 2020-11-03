{ sources ? import ./sources.nix }:
let
  overlay = _: pkgs: {
    niv = (import sources.niv {}).niv;
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
