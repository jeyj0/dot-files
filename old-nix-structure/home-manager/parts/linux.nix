{ pkgs }:
{
  imports = [
    (import ./gui-and-media { pkgs = pkgs; })
    (import ./zathura { pkgs = pkgs; })
    (import ./vscode { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    nixos-generators

    # nur.repos.rycee.firefox-addons-generator
    # nur.repos.rycee.mozilla-addons-to-nix

    sxiv
    fontpreview
    evince # for printing and pdf forms

    gnome.file-roller

    xclip
    xorg.xwininfo
    xorg.xmodmap
    lm_sensors

    networkmanager
    networkmanager_dmenu

    lxc

    # wonderdraft

    unstable.chromium
    # qutebrowser
    responsively

    unstable.signal-desktop

    appimage-run
    steam-run

    lychee-slicer
    blender
  ];
}
