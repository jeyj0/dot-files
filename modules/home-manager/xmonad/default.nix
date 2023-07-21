{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.xmonad = {
    enable = mkEnableOption "xmonad";
  };

  config = mkIf config.jeyj0.xmonad.enable {
    xsession.windowManager.xmonad = {
      enable = true;
      config = null; # just use package
    #   enableContribAndExtras = true;
    #   package = pkgs.jeyj0.xmonad;
    };

    home.packages = with pkgs; [
      jeyj0.xmonad
      nitrogen
    ];
  };
}