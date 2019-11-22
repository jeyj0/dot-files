#!/usr/bin/env zsh

mavenVersions=($HOME/maven/*)
newestMaven=${mavenVersions[-1]}
export PATH=$PATH:$newestMaven/bin
