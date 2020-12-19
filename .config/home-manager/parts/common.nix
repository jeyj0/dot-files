{ pkgs }:
{
  imports = [
    (import ./cli-tools { pkgs = pkgs; })
    (import ./neovim { pkgs = pkgs; })
    (import ./emacs { pkgs = pkgs; })
    (import ./kakoune { pkgs = pkgs; })
    (import ./docker { pkgs = pkgs; })
    (import ./webdev { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    openssl

    cachix
    lxc

    alacritty
    # zerotierone
  ];
}
