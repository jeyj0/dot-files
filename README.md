# dot-files
This is my personal .dot-files repository, where all my custom dotfiles and installation scripts go. 

To use it, clone the repo directly into the home directory.

## Installing NixOS-config:
1. Clone the repo into the home directory.
2. run `sudo ./nixos/create_link.sh`
3. rebuild NixOS with `sudo nixos-rebuild switch` (a reboot might be necessary)
4. ...profit
