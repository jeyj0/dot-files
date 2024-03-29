{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.git = {
    enable = mkEnableOption "git";
  };

  config = mkIf config.jeyj0.git.enable {
    home.packages = with pkgs.unstable; [
      git
      gitAndTools.diff-so-fancy # pretty diffs
    ];
    home.file.gitConfigs = {
      source = ./xdg-config;
      target = ".config/git";
      recursive = true;
    };
    home.sessionVariables = mkMerge [
      (mkIf config.jeyj0.neovim.enable { GIT_EDITOR = mkOverride 1002 "nvim"; })
      (mkIf config.jeyj0.helix.enable { GIT_EDITOR = mkOverride 1001 "hx"; })
    ];
  };
}
