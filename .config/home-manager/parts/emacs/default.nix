{ pkgs, installEmacs ? true }:
{
  home.packages = with pkgs; [
    ripgrep
    fd
    clang
    sqlite # for org-roam

    plantuml
  ] ++ (if installEmacs then [ pkgs.emacs ] else []);
}
