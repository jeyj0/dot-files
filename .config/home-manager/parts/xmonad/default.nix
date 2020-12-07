{ pkgs }:
{
  home.packages = with pkgs; [
    xmobar
    xdotool
    battery-status

    nitrogen # to set wallpapers
  ];
}
