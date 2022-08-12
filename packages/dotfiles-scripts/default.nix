{ symlinkJoin
, pkgs
, ...
}@inputs:
let
  apply-home = pkgs.callPackage
    ({ writeShellApplication
    , pkgs
    , ...
    }:
    writeShellApplication {
      name = "dot-apply-home";
      runtimeInputs = with pkgs; [
        bash
      ];
      text = ./apply-home.sh;
    }) inputs;

  apply-system = pkgs.callPackage
    ({ writeShellApplication
    , pkgs
    , ...
    }:
    writeShellApplication {
      name = "dot-apply-system";
      runtimeInputs = with pkgs; [
        bash
      ];
      text = ./apply-system.sh;
    }) inputs;

  apply = pkgs.callPackage
    ({ writeShellApplication
    , pkgs
    , ...
    }:
    writeShellApplication {
      name = "dot-apply";
      runtimeInputs = with pkgs; [
        bash
        apply-home
        apply-system
      ];
      text = ./apply.sh;
    }) inputs;

  build-home = pkgs.callPackage
    ({ writeShellApplication
    , pkgs
    , ...
    }:
    writeShellApplication {
      name = "dot-build-home";
      runtimeInputs = with pkgs; [
        bash
      ];
      text = ./build-home.sh;
    }) inputs;

  build-system = pkgs.callPackage
    ({ writeShellApplication
    , pkgs
    , ...
    }:
    writeShellApplication {
      name = "dot-build-system";
      runtimeInputs = with pkgs; [
        bash
      ];
      text = ./build-system.sh;
    }) inputs;

  update = pkgs.callPackage
    ({ writeShellApplication
    , pkgs
    , ...
    }:
    writeShellApplication {
      name = "dot-update";
      runtimeInputs = with pkgs; [
        bash
      ];
      text = ./update.sh;
    }) inputs;

  shell = pkgs.callPackage
    ({ writeShellApplication
    , pkgs
    , ...
    }:
    writeShellApplication {
      name = "dot-shell";
      runtimeInputs = with pkgs; [
        bash
      ];
      text = ./shell.sh;
    }) inputs;
in
symlinkJoin {
  name = "dotfiles-scripts";
  paths = [
    apply-home
    apply-system
    apply
    build-home
    build-system
    update
    shell
  ];
}

