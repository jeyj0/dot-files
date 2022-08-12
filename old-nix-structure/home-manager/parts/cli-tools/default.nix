{ pkgs }:
{
  home.packages = with pkgs; [
    curl
    wget
    htop

    unstable.gitui

    # pijul

    entr

    # ranger
      # highlight # highlight enables syntax highlighting in previews
    nnn
    jq
    # bat
    # exa
    # trash-cli
    zip unzip
    gnumake

    # starship # beautiful command prompt

    fzf
    # silver-searcher # ag (faster ack (faster & better grep))

    tldr # tl;dr man page alternative

    speedtest-cli
    
    asciinema
  ];
}
