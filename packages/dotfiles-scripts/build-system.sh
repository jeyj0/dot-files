#!/bin/sh
pushd ~
nixos-rebuild build --flake .#
popd
