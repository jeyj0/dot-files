{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.collections.core-gui = {
    enable = mkEnableOption "collection-core-gui";
  };

  config = mkIf config.jeyj0.collections.core-gui.enable {
    jeyj0 = {
      alacritty.enable = mkDefault true;
      firefox.enable = mkDefault true;
      obsidian.enable = mkDefault true;
      signal.enable = mkDefault true;
      vscode.enable = mkDefault true;
    };
  };
}

