{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.helix = {
    enable = mkEnableOption "helix";
  };

  config = mkIf config.jeyj0.helix.enable {
    programs.helix = {
      enable = true;
      package = pkgs.unstable.helix;
      settings = {
        theme = "gruvbox_contrast"
        keys.normal = {
          space = {
            space = "file_picker";
            f = {
              s = ":write"
            };
          };
        };
      };
    };
  };
}

