#!/usr/bin/env zsh
if [[ $(nvr --serverlist) ]]; then
  if [[ $# -ge 1 ]]; then
    nvr -cc split "$@"
  else
    echo "An instance is already open"
  fi
else
  nvr "$@"
fi
