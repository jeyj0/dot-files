#!/bin/sh
pushd ~/projects/dotfiles
nix --extra-experimental-features "nix-command flakes" build .#homeManagerConfigurations.jeyj0.activationPackage
./result/activate
popd
