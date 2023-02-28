{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.gimp = {
    enable = mkEnableOption "gimp";
  };

  config = mkIf config.jeyj0.gimp.enable {
    home.packages = with pkgs.unstable; [ gimp ];
  };
}