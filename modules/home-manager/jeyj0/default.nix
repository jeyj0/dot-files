{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0 = {
    enable = mkEnableOption "jeyj0";
    hostName = mkOption {
      type = types.str;
      description = ''
        The hostname this home configuration is made for.
      '';
    };
  };

  config = mkIf config.jeyj0.enable {
    home = {
      username = "jeyj0";
      homeDirectory = "/home/jeyj0";
      stateVersion = "22.05";
    };
  };
}
