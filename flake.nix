{
  description = "jeyj0's main flake. Contains packages and system, and home-manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";

    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    solbera-dnd-fonts.url = "github:jonathonf/solbera-dnd-fonts";
    solbera-dnd-fonts.flake = false;
  };

  outputs =
    { nixpkgs
    , home-manager
    , nur
    , emacs-overlay
    , solbera-dnd-fonts
    , ... }:
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = {
        allowUnfree = true;
      };
      overlays = [
        (import emacs-overlay)
        nur.overlay
        (_: pkgs: {
          firefox-addons = pkgs.callPackage (import ./nixos/nix/pkgs/firefox-addons) {};

          # responsively = pkgs.callPackage (import ./nixos/nix/pkgs/responsively.nix) {};
          nnn = pkgs.callPackage (import ./nixos/nix/pkgs/nnn.nix) {};

          dnd-fonts = pkgs.callPackage (import ./nixos/nix/pkgs/dnd-fonts.nix) {};

          # picom = pkgs.callPackage (import ./nixos/nix/pkgs/picom) {};
          # wonderdraft = pkgs.callPackage (import ./nixos/nix/pkgs/wonderdraft) {};
          lychee-slicer = pkgs.callPackage (import ./nixos/nix/pkgs/lychee-slicer) {};
        })
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
            ./.config/home-manager/jeyj0-nixos.nix
          ];
        };

      };
    };

    nixosConfigurations = {
      jeyj0-nixos = lib.nixosSystem {
        inherit system pkgs;

        modules = [
          ./nixos/hosts/jeyj0-nixos.nix
        ];
      };
    };

  };
}
