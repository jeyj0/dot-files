{ pkgs }:
{
  home.packages = with pkgs; [
    nur.repos.rycee.firefox-addons-generator

    xclip
    # xorg.xwininfo
    # xorg.xmodmap

    sxiv
    # apvlv
    # evince

    networkmanager
    networkmanager_dmenu
  ];
}
