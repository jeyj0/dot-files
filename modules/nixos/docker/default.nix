{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.docker = {
    enable = mkEnableOption "docker";
  };

  config = mkIf config.jeyj0.docker.enable {
    virtualisation.docker.enable = true;
  };
}