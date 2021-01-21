#!/usr/bin/env bash

set -e

ag '#\+roam_tags:.* npc($| .*)' ~/org/roam/dnd/ -l
