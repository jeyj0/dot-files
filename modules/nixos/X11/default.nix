{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.X11 = {
    enable = mkEnableOption "X11";
  };

  config = mkIf config.jeyj0.X11.enable {
    # Enable the X11 windowing system.
    services.xserver.enable = true;

    # Configure keymap in X11
    services.xserver = {
      layout = "us";
      xkbVariant = "";
    };
  };
}