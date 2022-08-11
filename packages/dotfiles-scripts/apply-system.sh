#!/bin/sh
pushd ~
sudo nixos-rebuild switch --flake .#
popd
