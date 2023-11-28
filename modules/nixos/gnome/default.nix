{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.gnome = {
    enable = mkEnableOption "gnome";
  };

  config = mkIf config.jeyj0.gnome.enable {
    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = true;
    # services.xserver.displayManager.gdm.wayland = false;
    services.xserver.desktopManager.gnome.enable = true;
  };
}
