{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.syncthing = {
    enable = mkEnableOption "syncthing";
  };

  config = mkIf config.jeyj0.syncthing.enable {
    services.syncthing = {
      enable = true;
      package = pkgs.unstable.syncthing;
    } // mkIf config.jeyj0.user-jeyj0.enable {
      user = "jeyj0";
      group = "jeyj0";
    };
  };
}