{ ... }:
{
  imports = [
    ./fonts
    ./settings
    ./gnome
    ./printing
    ./sound
    ./docker
    ./X11
    ./networkmanager
    # dotgen nixos module marker
  ];
}
