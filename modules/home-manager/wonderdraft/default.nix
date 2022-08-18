{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.wonderdraft = {
    enable = mkEnableOption "wonderdraft";
  };

  config = mkIf config.jeyj0.wonderdraft.enable {
    home.packages = with pkgs; [ wonderdraft ];
  };
}