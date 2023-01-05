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
    };
    home.file.alacrittyConfig = {
      text = builtins.replaceStrings
        ["%%FONT_SIZE%%"]
        ["${builtins.toString config.jeyj0.alacritty.fontSize}"]
        (builtins.readFile ./xdg-config/alacritty.yml);
      target = ".config/alacritty/alacritty.yml";
    };
  };
}
