{
  description = "jeyj0's main flake. Contains packages and system, and home-manager configurations";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    node2nix.url = "github:svanderburg/node2nix";

    helix.url = "github:helix-editor/helix";
    nil.url = "github:oxalica/nil";

    typst-lsp.url = "github:nvarner/typst-lsp/75ba918c119cff83a2ef2ed2d2f73ed3231ac6a2";

    typst.url = "github:typst/typst";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nvim.url = "github:neovim/neovim?dir=contrib";
    # nvim.inputs.nixpkgs.follows = "nixpkgs-unstable";

    nur.url = "github:nix-community/NUR";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    solbera-dnd-fonts.url = "github:jonathonf/solbera-dnd-fonts";
    solbera-dnd-fonts.flake = false;
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , node2nix
    , helix
    , nil
    , typst-lsp
    , typst
    , nixos-hardware
    , nvim
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
          inherit inputs;
          jeyj0 = self.packages.${system} // {
            neovim = nvim.packages.${system}.default;
            helix = helix.packages.${system}.default;
            nil = nil.packages.${system}.default;
            typst-lsp = typst-lsp.packages.${system}.default;
            typst = typst.packages.${system}.default;
          };
        })
        (_: _: {
          firefox-addons = self.firefox-addons;
        })
      ];
    };

    lib = nixpkgs.lib;

    framework = "jeyj0-framework";
    desktop = "jeyj0-nixos";
  in {
    nixosConfigurations = {
      "${desktop}" = lib.nixosSystem {
        inherit system pkgs;

        modules = [
          ./old-nix-structure/nixos/hosts/jeyj0-nixos.nix
          ./modules/nixos
          ({ ... }: {
            jeyj0 = {
              printing.enable = true;
              syncthing.enable = true;
            };
          })
        ];
      };

      "${framework}" = lib.nixosSystem {
        inherit system pkgs;

        modules = [
          nixos-hardware.nixosModules.framework
          ./modules/nixos
          ({ config, ... }: {
            networking.hostName = framework;

            # This value determines the NixOS release from which the default
            # settings for stateful data, like file locations and database versions
            # on your system were taken. It‘s perfectly fine and recommended to leave
            # this value at the release version of the first install of this system.
            # Before changing this value read the documentation for this option
            # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
            system.stateVersion = "22.05"; # Did you read the comment?

            imports = [ ./framework-configuration.nix ];
            boot.kernelPackages = pkgs.linuxPackages_latest;
            boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
            boot.kernelModules = [ "v4l2loopback" ];

            jeyj0 = {
              settings.enable = true;
              fonts.enable = true;
              gnome.enable = true;
              printing.enable = true;
              sound.enable = true;
              docker.enable = true;
              X11.enable = true;
              networkmanager.enable = true;
              user-jeyj0.enable = true;
              syncthing.enable = true;
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

      dotgen = pkgs.unstable.callPackage (import ./packages/dotgen) {};
      dotfiles-scripts = pkgs.unstable.callPackage (import ./packages/dotfiles-scripts) {};
      xmonad = pkgs.unstable.callPackage (import ./packages/xmonad) {};
      node-packages = pkgs.unstable.callPackage (import ./packages/node-packages) {};
      helix-tree-sitter-typst = pkgs.unstable.callPackage (import ./packages/helix-tree-sitter-typst) {};
      # TODO lychee-slicer fails (at runtime) with unstable
      lychee-slicer = pkgs.callPackage (import ./packages/lychee-slicer) {}; 
      # dotgen package marker

      homeConfigurations = {
        "jeyj0@${desktop}" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            ./old-nix-structure/home-manager/jeyj0-nixos.nix
            ./modules/home-manager
            ./collections/home-manager
            {
              jeyj0 = {
                enable = true;
                hostName = desktop;

                collections = {
                  core.enable = true;
                  core-gui.enable = true;
                };

                "microsoft-edge".enable = true;
                lychee-slicer.enable = true;

                # overridden from core-gui
                firefox.enable = false; # override because it's controlled from old nix structure

                freetube.enable = true;

                polybar.enable = true;
                obsidian.enable = true;
                picom.enable = true;
                thunar.enable = true;
                rofi.enable = true;
                alacritty = {
                  enable = true;
                  fontSize = 14.0;
                };
                # wonderdraft.enable = true;
                freecad.enable = true;
                xfconf.enable = true;
                syncthing.enable = true;
                typst.enable = true;
              };
            }
          ];
        };

        "jeyj0@${framework}" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./modules/home-manager
            ./collections/home-manager
            {
              jeyj0 = {
                enable = true;
                hostName = framework;

                collections = {
                  core.enable = true;
                  core-gui.enable = true;
                };

                gnome.enable = true;

                spotify.enable = true;
                slack.enable = true;
                discord.enable = true;
                zoom.enable = true;
                obs-studio.enable = true;
                gimp.enable = true;
                freecad.enable = true;
                freetube.enable = true;
                typst.enable = true;

                syncthing.enable = true;
              };
            }
          ];
        };
      };
    };

    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        dotgen
        dotfiles-scripts
        node2nix.packages.${system}.default
      ];
    };

  };
}

