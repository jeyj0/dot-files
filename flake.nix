{
  description = "jeyj0's main flake. Contains packages and system, and home-manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    solbera-dnd-fonts.url = "github:jonathonf/solbera-dnd-fonts";
    solbera-dnd-fonts.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , nur
    , emacs-overlay
    , solbera-dnd-fonts
    , ... }:
  let
    system = "x86_64-linux";

    config = {
      allowUnfree = true;
    };

    pkgs = import nixpkgs {
      inherit system config;
      overlays = [
        (import emacs-overlay)
        nur.overlay
        (final: prev: {
          unstable = import nixpkgs-unstable {
            inherit system config;
          };
        })
        (_: pkgs: self.packages)
      ];
    };

    lib = nixpkgs.lib;

  in {
    homeManagerConfigurations = {
      jeyj0 = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "jeyj0";
        homeDirectory = "/home/jeyj0";
        configuration = {
          imports = [
            ./old-nix-structure/home-manager/jeyj0-nixos.nix
          ];
        };

      };
    };

    nixosConfigurations = {
      jeyj0-nixos = lib.nixosSystem {
        inherit system pkgs;

        modules = [
          ./old-nix-structure/nixos/hosts/jeyj0-nixos.nix
        ];
      };
    };

    packages = {
      firefox-addons = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/firefox-addons) {};

      responsively = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/responsively.nix) {};
      nnn = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/nnn.nix) {};

      dnd-fonts = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/dnd-fonts.nix) {};

      picom = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/picom) {};
      # wonderdraft = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/wonderdraft) {};
      lychee-slicer = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/lychee-slicer) {};
    };

  };
}
