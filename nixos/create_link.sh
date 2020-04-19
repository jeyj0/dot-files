#!/usr/bin/env bash

set -o errexit

dir=$(pwd)

host=$(cat "${dir}/nixos/hosts/list" | fzf)

# cp /etc/nixos/configuration.nix "${dir}/nixos/configuration_local.nix"
ln -fs "${dir}/nixos/hosts/${host}.nix" "/etc/nixos/configuration.nix"
