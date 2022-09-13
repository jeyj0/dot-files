{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.slack = {
    enable = mkEnableOption "slack";
  };

  config = mkIf config.jeyj0.slack.enable {
    home.packages = with pkgs.unstable; [ slack ];
  };
}