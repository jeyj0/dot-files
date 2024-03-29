inputs: { ... }:
{
  imports = [
    ./polybar
    ./git
    ./obsidian
    ./picom
    ./starship
    ./fish
    ./starship
    ./neovim
    ./exa
    ./bat
    ./trash
    ./alacritty
    ./ag
    ./direnv
    ./helix
    ./qutebrowser
    ./thunar
    ./rofi
    ./wonderdraft
    ./freecad
    ./vscode
    ./clifm
    ./spotify
    ./slack
    ./firefox
    ./htop
    ./sshfs
    ./signal
    ./zoom
    ./xfconf
    ./jeyj0
    ./syncthing
    ./discord
    ./gnome
    ./gimp
    ./microsoft-edge
    ./obs-studio
    ./nix
    ./freetube
    ./home-manager
    ./typst
    ./lychee-slicer
    (import ./nix-index inputs)
    ./xmonad
    # dotgen home module marker
  ];
}

