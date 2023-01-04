{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.printing = {
    enable = mkEnableOption "printing";
  };

  config = mkIf config.jeyj0.printing.enable {
    services.printing = {
      enable = true;
      browsing = true;
      defaultShared = true;
      drivers = with pkgs; [
        hplip
      ];
    };
  };
}