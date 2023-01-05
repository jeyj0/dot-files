{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.alacritty = {
    enable = mkEnableOption "alacritty";
    fontSize = mkOption {
      type = types.float;
      default = 16.0;
    };
  };

  config = mkIf config.jeyj0.alacritty.enable {
    programs.alacritty = {
      enable = true;
      package = pkgs.unstable.alacritty;
      settings = {
        font = {
          size = config.jeyj0.alacritty.fontSize;
          normal = { family = "Hack Nerd Font"; style = "Regular"; };
          bold = { family = "Hack Nerd Font"; style = "Bold"; };
          italic = { family = "Hack Nerd Font"; style = "Italic"; };
          bold_italic = { family = "Hack Nerd Font"; style = "Bold Italic"; };
        };
        colors = {
          primary = {
            background = "#1c1c1c";
            foreground = "#ebdbb2";
          };
          normal = {
            black = "#282828";
            red = "#cc241d";
            green = "#98971a";
            yellow = "#d79921";
            blue = "#458588";
            magenta = "#b16286";
            cyan = "#689d6a";
            white = "#ebdbb2";
          };
        };
      };
    };
  };
}
