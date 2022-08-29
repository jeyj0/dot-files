{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.obsidian = {
    enable = mkEnableOption "obsidian";
  };

  config = mkIf config.jeyj0.obsidian.enable {
    home.packages = with pkgs.unstable; [ obsidian ];
    home.file.obsidianDesktopEntry = {
      source = ./obsidian.desktop;
      target = ".local/share/applications/obsidian.desktop";
    };
  };
}
