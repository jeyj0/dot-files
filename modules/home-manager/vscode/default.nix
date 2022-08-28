{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.vscode = {
    enable = mkEnableOption "vscode";
  };

  config = mkIf config.jeyj0.vscode.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.unstable.vscodium;
    };
  };
}
