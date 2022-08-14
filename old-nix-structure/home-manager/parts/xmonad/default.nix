{ pkgs }:
{
  imports = [
    (import ../dunst { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    xmobar
    xdotool

    nitrogen # to set wallpapers
    # TODO add picom module to xmonad collection when it's created
    # picom # for animations and other fancy stuff

    # rofi # TODO add rofi to xmonad collection when it's created

    libnotify # to actually send notifications
  ];

  home.file.xmonad-hs = {
    source = ./xmonad.hs;
    target = ".xmonad/xmonad.hs";

    # if config changed, try recompiling and restart on success
    onChange = "xmonad --recompile && xmonad --restart";
  };
}
