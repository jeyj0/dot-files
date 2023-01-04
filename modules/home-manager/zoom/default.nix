{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.zoom = {
    enable = mkEnableOption "zoom";
  };

  config = mkIf config.jeyj0.zoom.enable {
    home.packages = with pkgs.unstable; [ zoom-us ];
  };
}