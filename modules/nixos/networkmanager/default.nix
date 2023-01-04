{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.networkmanager = {
    enable = mkEnableOption "networkmanager";
  };

  config = mkIf config.jeyj0.networkmanager.enable {
    # Enable networking
    networking.networkmanager.enable = true;
  };
}