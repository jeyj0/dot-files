{ pkgs }:
let
  pythonPackages = pypkgs: with pypkgs; [
    pynvim
  ];
  pythonWithPackages = pkgs.python3.withPackages pythonPackages;
in
{
  home.packages = with pkgs; [
    # nodejs
    nodejs-12_x
    nodePackages.eslint

    # emacs
      ripgrep
      fd
      clang
      sqlite # for org-roam

    # vscodium

    # rustup
    # wasm-pack

    # basic utilities
    curl
    wget
    git
    gitAndTools.diff-so-fancy # pretty diffs
    # gitAndTools.gh # github cli
    # lazygit
    trash-cli
    silver-searcher # ag (faster ack (faster & better grep))
    htop
    entr
    # lsof
    direnv # for project-wise environments
    zip unzip
    tldr # tl;dr man page alternative
    docker-compose

    # google-cloud-sdk
    # kubectl

    # programs
    ## command line
    neovim-unwrapped
      watchman # watchman for coc.nvim
      neovim-remote # enable the use of only one neovim instance
      pythonWithPackages
    nnn
    # ranger
      # highlight # highlight enables syntax highlighting in previews
    fzf
    starship # beautiful command prompt
    bat
    exa
    # tuir
    cachix
    speedtest-cli
    # fontpreview
    haskellPackages.hoogle
    jq
    plantuml
    # texlive.combined.scheme-full

    # slack
    # discord
    # spotify

    # minecraft # while this should work, the package is currently broken
    # multimc

    # terminal emulators
    alacritty
    # kitty

    ## other
    # chromium
    # qutebrowser
    # gimp

    # obs-studio
    # obs-v4l2sink
    # ffmpeg
    # vlc

    # zoom-us

    # nextcloud-client
    # libreoffice
    # postman
    openssl

    # godot

    # elmPackages.elm-language-server

    # responsively

    # zerotierone

    ## Games
    # brogue

    # cloudflared
  ];
}
