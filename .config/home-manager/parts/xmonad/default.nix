{ pkgs }:
{
  home.packages = with pkgs; [
    xmobar
    xdotool
    battery-status

    nitrogen # to set wallpapers
    picom # for animations and other fancy stuff

    dunst # notification daemon
    libnotify # to actually send notifications
  ];

  home.file.xmonad-hs = {
    source = ./xmonad.hs;
    target = ".xmonad/xmonad.hs";

    # if config changed, try recompiling and restart on success
    onChange = "xmonad --recompile && xmonad --restart";
  };
}
