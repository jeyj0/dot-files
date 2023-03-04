{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.microsoft-edge = {
    enable = mkEnableOption "microsoft-edge";
  };

  config = mkIf config.jeyj0.microsoft-edge.enable {
    home.packages = with pkgs.unstable; [ microsoft-edge ];
  };
}