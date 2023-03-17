{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.obs-studio = {
    enable = mkEnableOption "obs-studio";
  };

  config = mkIf config.jeyj0.obs-studio.enable {
    programs.obs-studio = {
      enable = true;
      package = pkgs.unstable.obs-studio;
    };
  };
}
