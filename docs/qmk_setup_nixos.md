# How to setup qmk for flashing crkbd on nixos

## Steps
1. create a working-directory
2. `$ lorri init`
3. add `avrdude dfu-util dfu-programmer gcc-arm-embedded` to `shell.nix`
4. create a python virtual environment
5. `$ pip install qmk`
6. `$ qmk setup` (clone: yes)
