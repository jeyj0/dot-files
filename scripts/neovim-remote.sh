#!/usr/bin/env zsh
if [[ $(nvr --serverlist) ]]; then
  if [[ $# -ge 1 ]]; then
    nvr --servername $(nvr --serverlist | sed 1q) -cc split "$@"
  else
    echo "An instance is already open"
  fi
else
  nvim "$@"
fi
