{ pkgs, installEmacs ? true }:
{
  imports = [
    (import ./fish {
      extraConfigPre = ''
        set -x NIX_PROFILES "/nix/var/nix/profiles/default $HOME/.nix-profile"
        set -x NIX_SSL_CERT_FILE "$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt"
        set -x NIX_PATH /nix $HOME/.nix-defexpr/channels
        set -x PATH $HOME/.nix-profile/bin $PATH
      '';
    })
    (import ./cli-tools { pkgs = pkgs; })
    (import ./neovim { pkgs = pkgs; })
    (import ./emacs {
      pkgs = pkgs;
      installEmacs = installEmacs;
    })
    (import ./kakoune { pkgs = pkgs; })
    (import ./latex { pkgs = pkgs; })
    (import ./docker { pkgs = pkgs; })
    (import ./webdev { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    openssl

    cachix

    alacritty
    # zerotierone
  ];
}
