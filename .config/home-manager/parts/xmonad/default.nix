{ pkgs }:
{
  home.packages = with pkgs; [
    xmobar
    xdotool
    battery-status

    nitrogen # to set wallpapers
    picom # for animations and other fancy stuff
  ];

  home.file.xmonad-hs = {
    source = ./xmonad.hs;
    target = ".xmonad/xmonad.hs";
  };
}
