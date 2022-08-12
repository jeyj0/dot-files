{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.trash = {
    enable = mkEnableOption "trash";
  };

  config = mkIf config.jeyj0.trash.enable {
    home.packages = with pkgs.unstable; [ trash-cli ];
  };
}