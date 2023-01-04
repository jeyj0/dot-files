{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.sshfs = {
    enable = mkEnableOption "sshfs";
  };

  config = mkIf config.jeyj0.sshfs.enable {
    home.packages = with pkgs.unstable; [ sshfs ];
  };
}