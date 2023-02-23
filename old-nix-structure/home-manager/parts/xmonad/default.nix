{ pkgs }:
{
  imports = [
    (import ../dunst { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    xmonad
    xmobar
    xdotool

    nitrogen # to set wallpapers
    # TODO add picom module to xmonad collection when it's created
    # picom # for animations and other fancy stuff

    # rofi # TODO add rofi to xmonad collection when it's created

    libnotify # to actually send notifications
  ];
}
