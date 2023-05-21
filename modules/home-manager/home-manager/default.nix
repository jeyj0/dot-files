{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.home-manager = {
    enable = mkEnableOption "home-manager";
  };

  config = mkIf config.jeyj0.home-manager.enable {
    programs.home-manager = {
      enable = true;
    };
  };
}
