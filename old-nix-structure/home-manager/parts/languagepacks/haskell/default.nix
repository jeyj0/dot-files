{ pkgs }:
{
  home.packages = with pkgs; [
    haskellPackages.hoogle
  ];
}
