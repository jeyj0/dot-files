{ writeShellApplication
, pkgs
}:
writeShellApplication {
  name = "dotgen";
  runtimeInputs = with pkgs; [
    bash
    coreutils # for printf, cat
    gnused # for sed
    gum # for easy nice interactivity
  ];
  text = ./dotgen.sh;
}

