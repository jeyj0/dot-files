# dot-files
This is my personal .dot-files repository, where all my custom dotfiles and installation scripts go. 

To use it, clone the repo directly into the home directory.

## Update home-manager configuration

```sh
# uses current username and hostname by default
home-manager switch --flake .
```

## Usage

1. Download/clone this repository
2. Have nix installed and flakes enabled
3. Use `nix develop` to enter a shell with all requirements
4. Use `dot-*` commands to build, apply, or update the contained configurations

## How to get around the git conflict

To download and build this repository, you need git (because nix flakes depends on it).

However, activating a home-manager configuration including git conflicts with a possible git installed via `nix-env -iA nixos.git`.

To solve this, you need to split building and activating the home-manager configuration into two steps, uninstalling the `nix-env` git in-between:

```sh
nix-env -iA git # in case you haven't already
git clone ... # clone this repository using git
nix --extra-experimental-features "nix-command flakes" develop # enter a nix shell (enabling required experimental nix features)
dot-build-home # build the home-manager configuration
nix-env -e git-minimal # remove the nix-env git again
./result/activate # activate the home-manager installation that includes git
```

## WIP Note

The `.config` and `old-nix-structure` folders are left-over from the old way I managed my dotfiles, which assumed this directory to be cloned into `~` directly.

I am slowly working through everything, starting by moving everything inside `.config` into `modules/home-manager/`. That is the last thing required before `~` does not have to be a home directory anymore.

Afterwards, everything inside `old-nix-structure` needs to be moved into `modules/` and `collections/`, as well as some other folders, most likely.
