{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.xfconf = {
    enable = mkEnableOption "xfconf";
  };

  config = mkIf config.jeyj0.xfconf.enable {
    xfconf.settings = {
      xsettings = {
        "Gtk/FontName" = "Atkinson Hyperlegible 12";
        "Gtk/MonospaceFontName" = "Hack 10";
        "Gtk/CursorThemeName" = "Adwaita";
        "Gtk/ToolbarStyle" = "icons";

        "Net/ThemeName" = "Adwaita-dark";
        "Net/IconThemeName" = "Adwaita";
        "Net/CursorBlink" = true;
        "Net/CursorBlinkTime" = 1380;

        "Xft/DPI" = 96;
        "Xft/Antialias" = 1;
      };
    };
  };
}
