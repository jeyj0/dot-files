{ pkgs, ... }:
let
  userName = "jeyj0";

  packages = import ../../nixos/nix/packages.nix {};

  pythonPackages = pypkgs: with pypkgs; [
    pynvim
  ];
  pythonWithPackages = packages.python3.withPackages pythonPackages;
in
{
  imports = [
    (import ./parts/firefox {
      pkgs = packages;
      userName = userName;
    })
  ];

  home.packages = with packages; [
    nur.repos.rycee.firefox-addons-generator

    # xmonad
    xmobar
    xdotool
    battery-status

    # nodejs
    nodejs-12_x
    nodePackages.eslint

    # emacs
      ripgrep
      fd
      clang
      sqlite # for org-roam

    vscodium

    rustup
    wasm-pack

    # basic utilities
    curl
    wget
    git
    gitAndTools.diff-so-fancy # pretty diffs
    gitAndTools.gh # github cli
    lazygit
    xclip
    trash-cli
    silver-searcher # ag (faster ack (faster & better grep))
    htop
    entr
    lsof
    direnv # for project-wise environments
    nitrogen # to set wallpapers
    compton # making windows fancy
    zip unzip
    tldr # tl;dr man page alternative
    docker-compose

    now-cli
    google-cloud-sdk
    kubectl

    # programs
    ## command line
    neovim-unwrapped
      watchman # watchman for coc.nvim
      neovim-remote # enable the use of only one neovim instance
      pythonWithPackages
    nnn
    ranger
      highlight # highlight enables syntax highlighting in previews
    fzf
    starship # beautiful command prompt
    xorg.xwininfo
    xorg.xmodmap
    networkmanager
    networkmanager_dmenu
    bat
    exa
    tuir
    cachix
    speedtest-cli
    fontpreview
    haskellPackages.hoogle
    jq
    plantuml
    texlive.combined.scheme-full

    slack
    discord
    spotify

    # minecraft # while this should work, the package is currently broken
    multimc

    # terminal emulators
    alacritty
    kitty

    ## other
    chromium
    qutebrowser
    sxiv
    gimp

    apvlv
    evince

    obs-studio
    obs-v4l2sink
    ffmpeg
    vlc

    zoom-us

    nextcloud-client
    libreoffice
    postman
    openssl

    godot

    elmPackages.elm-language-server

    responsively

    zerotierone

    ## Games
    brogue

    cloudflared
  ];
}

