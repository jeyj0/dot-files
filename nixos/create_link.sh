#!/usr/bin/env bash

set -o errexit

dir=$(pwd)

cp /etc/nixos/configuration.nix "${dir}/nixos/configuration_local.nix"
ln -fs "${dir}/nixos/configuration.nix" /etc/nixos/configuration.nix
