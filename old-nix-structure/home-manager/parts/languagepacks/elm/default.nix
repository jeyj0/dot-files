{ pkgs }:
{
  home.packages = with pkgs; [
    elmPackages.elm-language-server
  ];
}
