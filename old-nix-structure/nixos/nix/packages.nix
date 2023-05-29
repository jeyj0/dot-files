{ sources ? import ./sources.nix }:
let
  overlay = _: pkgs: {
    niv = (import sources.niv {}).niv;

    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/2b95bf74192e883bb9088382f91a623c75cd717b.tar.gz";
      sha256 = "0mizrajd6w1387py1gblr2wrb8ma1hp3whpanbkcg27jkqg0lap1";
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
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/fb572c671b81c7f87c9e911e859a708278a33fa3.tar.gz;
      }))
    ];
    config = {
      allowUnfree = true;
    };
  }
