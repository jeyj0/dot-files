{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.ag = {
    enable = mkEnableOption "ag";
  };

  config = mkIf config.jeyj0.ag.enable {
    home.packages = with pkgs.unstable; [ silver-searcher ];
  };
}