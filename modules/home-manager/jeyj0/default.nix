{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0 = {
    enable = mkEnableOption "jeyj0";
  };

  config = mkIf config.jeyj0.enable {
    home = {
      username = "jeyj0";
      homeDirectory = "/home/jeyj0";
      stateVersion = "22.05";
    };
  };
}
