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
      sessionVariables = mkMerge [
        {
          IHP_BROWSER="echo";
        }
        (mkIf config.jeyj0.neovim.enable {
          EDITOR = mkOverride 1002 "nvim";
          VISUAL = mkOverride 1002 "nvim";
        })
        (mkIf config.jeyj0.helix.enable {
          EDITOR = mkOverride 1001 "hx";
          VISUAL = mkOverride 1001 "hx";
        })
      ];
    };

    jeyj0.nix.enable = true;
  };
}
