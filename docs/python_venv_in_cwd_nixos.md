# How to setup a python virtual env under NixOS

## Prerequisites
- lorri

## Steps
1. `$ lorri init`
2. add `python37full` to `shell.nix`
3. create a virtual python environment: `$ python -m venv .venv` (creates it in ./.venv/)
4. add `source ./.venv/bin/activate` to `.envrc` (`$ echo 'source
   ./.venv/bin/activate' >> .envrc`)
5. (optional) remove python from `shell.nix` (since we're now using the virtual
   environment, it's unnecessary)
6. (optional) remove the `shell.nix` and `eval "$(lorri direnv)"` from
   `.envrc` - it was only used to create the virtual environment, which now
   works by itself.

