{ pkgs, extraFishConfigPre ? "" }:
{
  imports = [
    (import ./fish {
      extraConfigPre = extraFishConfigPre;
    })
    (import ./cli-tools { pkgs = pkgs; })
    (import ./neovim { pkgs = pkgs; })
    (import ./emacs { pkgs = pkgs; })
    (import ./kakoune { pkgs = pkgs; })
    (import ./latex { pkgs = pkgs; })
    (import ./docker { pkgs = pkgs; })
    (import ./communication { pkgs = pkgs; })
    (import ./webdev { pkgs = pkgs; })
  ];

  home.packages = with pkgs; [
    openssl

    cachix

    alacritty
    # zerotierone

    ngrok
    curlie

    sc-im
  ];
}

