{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.starship = {
    enable = mkEnableOption "starship";
  };

  config = mkIf config.jeyj0.starship.enable {
    programs.starship = {
      enable = true;
      package = pkgs.unstable.starship;
      enableFishIntegration = config.jeyj0.fish.enable;
      settings = {
        add_newline = false;
        directory = {
          truncation_length = 1;
        };
        nix_shell = {
          disabled = true;
        };
        gcloud = {
          disabled = true;
        };
      };
    };
  };
}

