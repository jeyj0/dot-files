{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.bat = {
    enable = mkEnableOption "bat";
  };

  config = mkIf config.jeyj0.bat.enable {
    programs.bat = {
      enable = true;
    };
  };
}