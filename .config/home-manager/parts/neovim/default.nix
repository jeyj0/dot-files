{ pkgs }:
let
  pythonPackages = pypkgs: with pypkgs; [
    pynvim
  ];
  pythonWithPackages = pkgs.python3.withPackages pythonPackages;
in
{
  home.packages = with pkgs; [
    neovim-unwrapped

    watchman # watchman for coc.nvim
    neovim-remote # enable the use of only one neovim instance
    pythonWithPackages

    # nodejs
    nodejs-12_x
    nodePackages.eslint

    fzf
    nnn
  ];
}
