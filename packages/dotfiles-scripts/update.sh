#!/bin/sh
pushd ~
nix flake update
popd
