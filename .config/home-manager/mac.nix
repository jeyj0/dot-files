{ userName, userHome }:
{ pkgs, ... }:
let
  packages = import ../../nixos/nix/packages.nix {};
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = userName;
  home.homeDirectory = userHome;

  imports = [
    (import ./parts/home-manager.nix)
    (import ./parts/common.nix {
      pkgs = packages;
      extraFishConfigPre = ''
        set -x NIX_PROFILES "/nix/var/nix/profiles/default $HOME/.nix-profile"
        set -x NIX_SSL_CERT_FILE "$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt"
        set -x NIX_PATH /nix $HOME/.nix-defexpr/channels
        set -x PATH $HOME/.nix-profile/bin $PATH
      '';
    })
    (import ./parts/karabiner-elements { pkgs = packages; })
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
