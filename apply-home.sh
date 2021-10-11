#!/bin/sh
pushd ~
nix build .#homeManagerConfigurations.jeyj0.activationPackage
./result/activate 
popd
