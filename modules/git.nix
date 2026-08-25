{ config
, pkgs
, lib
, ...
}:
with lib;
{
  home.packages = with pkgs; [
    git
    diff-so-fancy # pretty diffs
  ];
  home.file.gitConfigs = {
    source = ../config/git;
    target = ".config/git";
    recursive = true;
  };
  home.sessionVariables = {
    GIT_EDITOR = "hx";
  };
}
