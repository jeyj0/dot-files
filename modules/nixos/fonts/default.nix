{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.fonts = {
    enable = mkEnableOption "fonts";
  };

  config = mkIf config.jeyj0.fonts.enable {
    fonts.fonts = with pkgs; [
      hack-font
      (nerdfonts.override { fonts = ["Hack"]; })
    ];
  };
}

