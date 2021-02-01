{ pkgs }:
{
  imports = [
    (import ./gui-and-media { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    nur.repos.rycee.firefox-addons-generator

    sxiv
    fontpreview
    zathura

    xclip
    xorg.xwininfo
    xorg.xmodmap

    networkmanager
    networkmanager_dmenu

    lxc

    wonderdraft

    chromium
    qutebrowser
    responsively
  ];
}
