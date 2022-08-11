{ pkgs }:
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    nix-direnv.enableFlakes = true;
  };

  home.packages = with pkgs; [
    curl
    wget
    htop

    # git
    gitAndTools.diff-so-fancy # TODO move | pretty diffs
    unstable.gitui

    # pijul

    entr

    # ranger
      # highlight # highlight enables syntax highlighting in previews
    nnn
    jq
    bat
    exa
    trash-cli
    zip unzip
    gnumake

    starship # beautiful command prompt

    fzf
    silver-searcher # ag (faster ack (faster & better grep))

    tldr # tl;dr man page alternative

    speedtest-cli
    
    asciinema
  ];
}
