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
, lib
, ...
}:
with lib;
{
  options.jeyj0.$MODULE_NAME = {
    enable = mkEnableOption \"$MODULE_NAME\";
  };

  config = mkIf config.jeyj0.$MODULE_NAME.enable {
    home.packages = with $PKGS; [ $PACKAGE_NAME ];
  };
}
"

    HM_MODULES_LIST_FILE="$ROOT_FOLDER/modules/home-manager/default.nix"

    echo ""

    echo "Planned Actions:"
    echo "Create directory: $FOLDER"
    echo "Create file: $FILE_PATH"
    echo "Add \"./$MODULE_NAME\" to import list in $HM_MODULES_LIST_FILE"

    CMDS="mkdir --parents \"$FOLDER\"
cat <<'EOF' >\"$FILE_PATH\"
$FILE_CONTENTS
EOF
sed --in-place \"s/^\(\s*\)# dotgen home module marker/\1.\/$MODULE_NAME\n\1# dotgen home module marker/\" \"$HM_MODULES_LIST_FILE\""
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

  echo "❓ What should be the package's name?"
  PACKAGE_NAME=$(gum input)
  echo "$PACKAGE_NAME"
  echo ""

  FOLDER="$ROOT_FOLDER/packages/$PACKAGE_NAME"
  FILE_PATH="$FOLDER/default.nix"
  FLAKE_NIX_PATH="$ROOT_FOLDER/flake.nix"
  FILE_CONTENTS="{ lib
, stdenv
, pkgs
, ...
}:
stdenv.mkDerivation {
  name = \"$PACKAGE_NAME\";
  src = ./.;
  installPhase = ''
    mkdir -p \$out/bin
    cat <<'EOF' > \$out/bin/hello
    #!/bin/sh
    echo \"Hello $PACKAGE_NAME!\"
    EOF
    chmod +x \$out/bin/hello
  '';
}
"
  PACKAGE_ENTRY="$PACKAGE_NAME = pkgs.unstable.callPackage (import .\/packages\/$PACKAGE_NAME) {};"

  echo "Planned Actions:"
  echo "Create directory: $FOLDER"
  echo "Create file: $FILE_PATH"
  echo "Add \"$PACKAGE_ENTRY\" to package list in $FLAKE_NIX_PATH"

  DOTGEN_MARKER="dotgen package marker"
  CMDS="mkdir --parents \"$FOLDER\"
cat <<'EOF' >\"$FILE_PATH\"
$FILE_CONTENTS
EOF
sed --in-place \"s/^\(\s*\)# $DOTGEN_MARKER/\1$PACKAGE_ENTRY\n\1# $DOTGEN_MARKER/\" \"$FLAKE_NIX_PATH\""
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

