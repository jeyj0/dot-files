{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.thunar = {
    enable = mkEnableOption "thunar";
  };

  config = mkIf config.jeyj0.thunar.enable {
    home.packages = with pkgs.unstable; [ xfce.thunar ];
    home.file.thunarConfigs = {
      source = ./xdg-config;
      target = ".config/Thunar";
      recursive = true;
    };
  };
}
