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
        key_bindings = [
          { key = "F11"; action = "ToggleFullscreen"; }
        ];
        font = {
          size = config.jeyj0.alacritty.fontSize;
          normal = { family = "Hack Nerd Font"; style = "Regular"; };
          bold = { family = "Hack Nerd Font"; style = "Bold"; };
          italic = { family = "Hack Nerd Font"; style = "Italic"; };
          bold_italic = { family = "Hack Nerd Font"; style = "Bold Italic"; };
        };
        colors = {
          # TokyoNight Alacritty Colors
          # Default colors
          primary = {
            background = "0x222436";
            foreground = "0xc8d3f5";
          };

          # Normal colors
          normal = {
            black =   "0x1b1d2b";
            red =     "0xff757f";
            green =   "0xc3e88d";
            yellow =  "0xffc777";
            blue =    "0x82aaff";
            magenta = "0xc099ff";
            cyan =    "0x86e1fc";
            white =   "0x828bb8";
          };

          # Bright colors
          bright = {
            black =   "0x444a73";
            red =     "0xff757f";
            green =   "0xc3e88d";
            yellow =  "0xffc777";
            blue =    "0x82aaff";
            magenta = "0xc099ff";
            cyan =    "0x86e1fc";
            white =   "0xc8d3f5";
          };

          indexed_colors = [
            { index = 16; color = "0xff966c"; }
            { index = 17; color = "0xc53b53"; }
          ];
        };
      };
    };
  };
}

