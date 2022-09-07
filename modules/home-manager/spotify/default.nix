{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.spotify = {
    enable = mkEnableOption "spotify";
  };

  config = mkIf config.jeyj0.spotify.enable {
    home.packages = with pkgs.unstable; [ spotify ];
  };
}