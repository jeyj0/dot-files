{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.picom = {
    enable = mkEnableOption "picom";
  };

  config = mkIf config.jeyj0.picom.enable {
    home.packages = with pkgs; [ picom ];
    home.file.picomConf = {
      source = ./picom.conf;
      target = ".config/picom.conf";
    };
  };
}

