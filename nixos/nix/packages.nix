{ sources ? import ./sources.nix }:
let
  overlay = _: pkgs: {
    niv = (import sources.niv {}).niv;

    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/cc3d8311e40485e881c56b159aa3c824b877a0e9.tar.gz";
      sha256 = "04jfh0hbcwxy6rdk054gnmfc0ig6sr0s5fhkwwg90gn8hgmgln8l";
    }) {
      inherit pkgs;
    };

    firefox-addons = pkgs.callPackage (import ./pkgs/firefox-addons) {};

    responsively = pkgs.callPackage (import ./pkgs/responsively.nix) {};
    nnn = pkgs.callPackage (import ./pkgs/nnn.nix) {};
    battery-status = import ../../scripts/battery-status;
    dnd-fonts = pkgs.callPackage (import ./pkgs/dnd-fonts.nix) {};
    picom = pkgs.callPackage (import ./pkgs/picom) {};
    wonderdraft = pkgs.callPackage (import ./pkgs/wonderdraft) {};
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
