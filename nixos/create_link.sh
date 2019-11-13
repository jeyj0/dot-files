#!/usr/bin/env bash

set -o errexit

dir=$(pwd)

ln -fs "${dir}/nixos/configuration.nix" /etc/nixos/configuration.nix
