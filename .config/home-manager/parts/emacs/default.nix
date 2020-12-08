{ pkgs }:
{
  imports = [
    (import ../latex { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    ripgrep
    fd
    clang
    sqlite # for org-roam

    plantuml
  ];
}
