{ config
, pkgs
}:
with lib;
{
  options.jeyj0.git = {
    enable = mkEnableOption "git";
  };

  config = mkIf config.jeyj0.git.enable = {
    home.packages = with pkgs.unstable; [ git ];
  };
}

