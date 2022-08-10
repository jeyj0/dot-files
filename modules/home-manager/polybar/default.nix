{ config, lib, pkgs, ... }:
with lib;
{
  options.jeyj0.polybar = {
    enable = mkEnableOption "polybar";
  };

  config = mkIf config.jeyj0.polybar.enable {
    services.polybar = {
      enable = true;
      script = "polybar desktop-left &";
      settings = {
        "bar/desktop-left" = {
          monitor = "DP-2";
          modules = {
            # left = "cpu ram";
            left = "cpu";
          };
          font = ["Hack Nerd Font"];
        };
        "module/cpu" = {
          type = "internal/cpu";
        };
      };
    };
  };
}

