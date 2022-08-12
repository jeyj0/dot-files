{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.qutebrowser = {
    enable = mkEnableOption "qutebrowser";
  };

  config = mkIf config.jeyj0.qutebrowser.enable {
    programs.qutebrowser = {
      enable = true;
      package = pkgs.unstable.qutebrowser;
      extraConfig = ''
        c.scrolling.smooth = True
        c.auto_save.session = True
        c.zoom.default = 150
      '';
    };
  };
}
