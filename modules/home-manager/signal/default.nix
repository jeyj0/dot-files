{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.signal = {
    enable = mkEnableOption "signal";
  };

  config = mkIf config.jeyj0.signal.enable {
    home.packages = with pkgs.unstable; [ signal-desktop ];
  };
}