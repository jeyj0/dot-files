{ pkgs }:
{
  home.packages = with pkgs; [
    chromium
    qutebrowser
    responsively
    # postman
  ];
}

