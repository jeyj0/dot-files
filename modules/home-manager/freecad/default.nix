{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.freecad = {
    enable = mkEnableOption "freecad";
  };

  config = mkIf config.jeyj0.freecad.enable {
    home.packages = with pkgs.unstable; [ freecad ];
  };
}
