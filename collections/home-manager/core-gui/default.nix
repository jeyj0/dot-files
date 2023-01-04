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
      alacritty.enable = true;
      firefox.enable = true;
      obsidian.enable = true;
      signal.enable = true;
      vscode.enable = true;
    };
  };
}

