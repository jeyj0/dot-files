{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.exa = {
    enable = mkEnableOption "exa";
  };

  config = mkIf config.jeyj0.exa.enable {
    programs.exa = {
      enable = true;
    };
  };
}