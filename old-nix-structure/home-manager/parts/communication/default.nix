{ pkgs }:
{
  home.packages = with pkgs; [
    unstable.slack
    # discord
  ];
}

