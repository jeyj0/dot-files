{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.freetube = {
    enable = mkEnableOption "freetube";
  };

  config = mkIf config.jeyj0.freetube.enable {
    home.packages = with pkgs.unstable; [ freetube ];
    # importable data saved in 03-resources
  };
}