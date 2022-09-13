{
  description = "jeyj0's main flake. Contains packages and system, and home-manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

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
    , nixos-hardware
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
        (_: _: self.packages.${system})
        (_: _: {
          firefox-addons = self.firefox-addons;
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
            ./old-nix-structure/home-manager/jeyj0-nixos.nix
            ./modules/home-manager
            ./collections/home-manager
          ];

          jeyj0.collections = {
            core.enable = true;
          };

          jeyj0 = {
            polybar.enable = true;
            obsidian.enable = true;
            picom.enable = true;
            thunar.enable = true;
            rofi.enable = true;
            alacritty.enable = true;
            # wonderdraft.enable = true;
            freecad.enable = true;
          };
        };

      };

      core = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "jeyj0";
        homeDirectory = "/home/jeyj0";
        configuration = {
          imports = [
            ./modules/home-manager
            ./collections/home-manager
          ];

          jeyj0.collections = {
            core.enable = true;
          };

          jeyj0 = {
            alacritty.enable = true;
            vscode.enable = true;
          };
        };
      };

      laptop = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "jeyj0";
        homeDirectory = "/home/jeyj0";
        configuration = {
          imports = [
            ./modules/home-manager
            ./collections/home-manager
          ];

          jeyj0.collections = {
            core.enable = true;
          };

          jeyj0 = {
            alacritty.enable = true;
            vscode.enable = true;
            obsidian.enable = true;
            spotify.enable = true;
            slack.enable = true;
          };
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

      framework = lib.nixosSystem {
        inherit system pkgs;

        modules = [
          # nixos-hardware.nixosModules.framework
          ./modules/nixos
          ({ ... }: {
            networking.hostName = "jeyj0-framework";

            # This value determines the NixOS release from which the default
            # settings for stateful data, like file locations and database versions
            # on your system were taken. It‘s perfectly fine and recommended to leave
            # this value at the release version of the first install of this system.
            # Before changing this value read the documentation for this option
            # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
            system.stateVersion = "22.05"; # Did you read the comment?

            jeyj0 = {
              unmigrated-framework-config.enable = true;
              settings.enable = true;
              fonts.enable = true;
            };
          })
        ];
      };
    };

    firefox-addons = pkgs.callPackage (./old-nix-structure/nixos/nix/pkgs/firefox-addons) {};

    packages.${system} = {
      responsively = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/responsively.nix) {};
      nnn = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/nnn.nix) {};

      dnd-fonts = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/dnd-fonts.nix) {};

      picom = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/picom) {};
      # wonderdraft = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/wonderdraft) {};
      lychee-slicer = pkgs.callPackage (import ./old-nix-structure/nixos/nix/pkgs/lychee-slicer) {};

      dotgen = pkgs.unstable.callPackage (import ./packages/dotgen) {};
      dotfiles-scripts = pkgs.unstable.callPackage (import ./packages/dotfiles-scripts) {};
      # dotgen package marker
    };

    devShell.${system} = pkgs.mkShell {
      packages = with pkgs; [
        dotgen
        dotfiles-scripts
      ];
    };

  };
}

