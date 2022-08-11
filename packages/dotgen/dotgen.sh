#!/usr/bin/env bash

ROOT_FOLDER="$HOME"

echo "❓ What do you want to create?"
TYPE=$(gum choose "home module" "system module" "package")
echo "$TYPE"
echo ""

if [ "$TYPE" = "home module" ]; then

  echo "❓ What type of module do you want?"
  MODULE_TYPE=$(gum choose "simple package")
  echo "$MODULE_TYPE"
  echo ""

  if [ "$MODULE_TYPE" = "simple package" ]; then

    echo "❓ What is the package's name in nixpkgs?"
    PACKAGE_NAME=$(gum input)
    echo "$PACKAGE_NAME"
    echo ""

    echo "❓ What do you want to call the module?"
    MODULE_NAME=$(gum input --placeholder "$PACKAGE_NAME")

    if [ "$MODULE_NAME" = "" ]; then
      MODULE_NAME=$PACKAGE_NAME
    fi
    echo "$MODULE_NAME"
    echo ""

    UNSTABLE_NIXPKGS=$(gum confirm "Use unstable nixpkgs?")

    if $UNSTABLE_NIXPKGS ; then
      PKGS="pkgs.unstable"
      echo "Using unstable nixpkgs"
    else
      PKGS="pkgs"
      echo "Using stable nixpkgs"
    fi
    echo ""

    FOLDER="$ROOT_FOLDER/modules/home-manager/$MODULE_NAME"
    FILE_PATH="$FOLDER/default.nix"

    FILE_CONTENTS="{ config
, pkgs
}:
with lib;
{
  options.jeyj0.$MODULE_NAME = {
    enable = mkEnableOption \"$MODULE_NAME\";
  };

  config = mkIf config.jeyj0.$MODULE_NAME.enable = {
    home.packages = with $PKGS; [ $PACKAGE_NAME ];
  };
}
"

    echo ""

    echo "Planned Actions:"
    echo "Create directory: $FOLDER"
    echo "Create file: $FILE_PATH"
    echo "Add \"./$MODULE_NAME\" to import list in $ROOT/modules/home-manager/default.nix"

    CMDS="mkdir --parents \"$FOLDER\"
cat <<'EOF' >\"$FILE_PATH\"
$FILE_CONTENTS
EOF
sed --in-place \"s/^\(\s*\)# dotgen home module marker/\1.\/$MODULE_NAME\n\1# dotgen home module marker/\" \"$ROOT_FOLDER/modules/home-manager/default.nix\""
    echo ""
    echo "Going to execute:"
    echo "$CMDS"
    echo ""

    if gum confirm "Run actions?" ; then
      bash -c "$CMDS"
    else
      echo "Exiting without applying changes"
    fi
  fi

elif [ "$TYPE" = "system module" ]; then
  echo "Sorry, haven't implemented that yet..."
elif [ "$TYPE" = "package" ]; then
  echo "Sorry, haven't implemented that yet..."
fi

