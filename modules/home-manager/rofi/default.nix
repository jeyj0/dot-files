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
      font = "Hack Nerd Font 14";
      theme = ./tokyonight.rasi;
    };
  };
}
