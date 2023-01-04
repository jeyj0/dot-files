{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.firefox = {
    enable = mkEnableOption "firefox";
  };

  config = mkIf config.jeyj0.firefox.enable {
    programs.firefox = {
      enable = true;
      package = pkgs.unstable.firefox;
    };
  };
}