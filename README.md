# dot-files
This is my personal .dot-files repository, where all my custom dotfiles and installation scripts go. 

To use it, clone the repo directly into the home directory.

## How to get around the git conflict

To download and build this repository, you need git (because nix flakes depends on it).

However, activating a home-manager configuration including git conflicts with a possible git installed via `nix-env -iA nixos.git`.

To solve this, you need to split building and activating the home-manager configuration into two steps, uninstalling the `nix-env` git in-between:

```sh
nix-env -iA git # in case you haven't already
git clone ... # clone this repository using git
./build-home.sh # build the home-manager configuration
nix-env -e git-minimal # remove the nix-env git again
./result/activate # activate the home-manager installation that includes git
```

## Installing NixOS-config:
1. Clone the repo into the home directory.
2. run `sudo ./nixos/create_link.sh`
3. rebuild NixOS with `sudo nixos-rebuild switch` (a reboot might be necessary)
4. ...profit
