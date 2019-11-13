#!/usr/bin/env zsh

set -o errexit

mkdir -p "${HOME}/.config/nvm"
git clone --depth=1 https://github.com/nvm-sh/nvm.git $HOME/.config/nvm

source $HOME/scripts/paths/nvm.sh # load nvm to use in this script

nvm install node
