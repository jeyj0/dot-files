{ pkgs }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      esbenp.prettier-vscode
      github.copilot
      github.vscode-pull-request-github
      ms-azuretools.vscode-docker

      # theme
      jdinhlife.gruvbox
    ];
  };
}
