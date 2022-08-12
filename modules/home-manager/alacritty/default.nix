{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.alacritty = {
    enable = mkEnableOption "alacritty";
  };

  config = mkIf config.jeyj0.alacritty.enable {
    programs.alacritty = {
      enable = true;
      package = pkgs.unstable.alacritty;
    };
    home.file.alacrittyConfigs = {
      source = ./xdg-config;
      target = ".config/alacritty";
      recursive = true;
    };
  };
}
