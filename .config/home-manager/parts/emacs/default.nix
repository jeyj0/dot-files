{ pkgs }:
let
  emacsConfig = ./emacs.el;
in
{
  programs.emacs = {
    enable = true;
    package = (pkgs.emacsWithPackagesFromUsePackage {
      config = emacsConfig;
      package = pkgs.emacs;
      alwaysEnsure = true;
    });
    extraPackages = emacsPkgs: with emacsPkgs; [
      use-package
    ];
  };

  home.packages = with pkgs; [
    ripgrep
    fd
    clang
    sqlite # for org-roam

    plantuml
  ];

  home.file.emacsConfig = {
    source = emacsConfig;
    target = ".emacs";
  };
}
