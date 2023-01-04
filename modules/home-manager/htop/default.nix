{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.htop = {
    enable = mkEnableOption "htop";
  };

  config = mkIf config.jeyj0.htop.enable {
    home.packages = with pkgs.unstable; [ htop ];
  };
}