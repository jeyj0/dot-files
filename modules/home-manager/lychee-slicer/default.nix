{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.lychee-slicer = {
    enable = mkEnableOption "lychee-slicer";
  };

  config = mkIf config.jeyj0.lychee-slicer.enable {
    home.packages = with pkgs; [ lychee-slicer ];
  };
}