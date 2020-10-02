# How to setup a python virtual env under NixOS

## Steps

1. create a virtual python environment:
   `$ nix-shell --pure -p python38Full --run "python -m venv .venv"`
   (creates it in ./.venv/)
2. add `source ./.venv/bin/activate` to `.envrc` (`$ echo 'source
   ./.venv/bin/activate' > .envrc`)
