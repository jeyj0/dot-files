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
      };
    };
  };
}
