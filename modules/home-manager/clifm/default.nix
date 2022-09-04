{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.clifm = {
    enable = mkEnableOption "clifm";
  };

  config = mkIf config.jeyj0.clifm.enable {
    home.packages = with pkgs.unstable; [ clifm ];
    home.file = {
      clifmDefaultProfileRc = {
        source = ./clifmrc;
        target = ".config/clifm/profiles/default/clifmrc";
      };
    };
  };
}
