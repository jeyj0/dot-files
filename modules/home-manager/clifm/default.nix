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
      clifmDefaultColorsWithStarshipPrompt = {
        source = ./default.cfm;
        target = ".config/clifm/colors/default-starship.clifm";
      };
      clifmDefaultProfileMimetypes = {
        source = ./mimelist.cfm;
        target = "/home/jeyj0/.config/clifm/profiles/default/mimelist.clifm";
      };
    };
  };
}
