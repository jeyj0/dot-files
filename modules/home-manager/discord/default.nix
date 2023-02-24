{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.discord = {
    enable = mkEnableOption "discord";
  };

  config = mkIf config.jeyj0.discord.enable {
    home.packages = with pkgs.unstable; [ discord ];
  };
}