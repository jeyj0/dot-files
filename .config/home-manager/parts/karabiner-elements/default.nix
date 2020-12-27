{ pkgs }:
{
  home.packages = with pkgs; [
    goku # a karabiner-elements configuration language
  ];

  home.file.gokuConfig = {
    onChange = "goku";
    source = ./karabiner.edn;
    target = ".config/karabiner.edn";
  };
}
