{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.rofi = {
    enable = mkEnableOption "rofi";
  };

  config = mkIf config.jeyj0.rofi.enable {
    programs.rofi = {
      enable = true;
      package = pkgs.unstable.rofi;
    };
    home.file.rofiConfigs = {
      source = ./xdg-config;
      target = ".config/rofi";
      recursive = true;
    };
  };
}
