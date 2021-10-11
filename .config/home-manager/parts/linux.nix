{ pkgs }:
{
  imports = [
    (import ./gui-and-media { pkgs = pkgs; })
    (import ./zathura { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    nur.repos.rycee.firefox-addons-generator

    sxiv
    fontpreview
    evince # for printing and pdf forms

    gnome.file-roller

    xclip
    xorg.xwininfo
    xorg.xmodmap

    networkmanager
    networkmanager_dmenu

    lxc

    # wonderdraft
    obsidian

    chromium
    # qutebrowser
    # responsively

    signal-desktop

    appimage-run
    steam-run

    lychee-slicer
    blender
  ];
}
