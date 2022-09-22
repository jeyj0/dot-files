{ pkgs }:
let
  pythonPackages = pypkgs: with pypkgs; [
    pynvim
  ];
  pythonWithPackages = pkgs.python3.withPackages pythonPackages;
in
{
  home.packages = with pkgs; [
    # unstable.neovim-unwrapped

    watchman # watchman for coc.nvim
    # neovim-remote # enable the use of only one neovim instance
    pythonWithPackages

    # nodejs
    unstable.nodejs
    nodePackages.eslint

    fzf
    nnn
  ];

  home.file = {
    # packerNvim = {
    #   source = pkgs.fetchFromGitHub {
    #     owner = "wbthomason";
    #     repo = "packer.nvim";
    #     rev = "4dedd3b08f8c6e3f84afbce0c23b66320cd2a8f2";
    #     sha256 = "sha256-dGmvrQOscGZ+Qk/RCJKJEOxUOcFrAHBGxpASNKZyWCc=";
    #   };
    #   target = ".config/nvim/site/pack/packer/start/packer.nvim";
    # };
  };
}
