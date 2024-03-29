{ pkgs }:
let
  emacsConfig = ./emacs.el;
  orgTransclusion = pkgs.fetchFromGitHub {
    owner = "nobiot";
    repo = "org-transclusion";
    rev = "c1b20436911f364c9477a17905af2881c765da14";
    sha256 = "0plb0wj461qb4crfbk6lsl45ixj0fz6xddcv46n5fs829d901hds";
  };
  orgRoam = pkgs.fetchFromGitHub {
    owner = "org-roam";
    repo = "org-roam";
    rev = "b183fd47bde6d61a6cef0ef041bfa596487190c3"; # v2
    sha256 = "1180j0yxzgc9r04jvqnh7yldnnm42liph4vmaf430dc8xnk3wgzz";
  };
in
{
  programs.emacs = {
    enable = true;
    package = (pkgs.emacsWithPackagesFromUsePackage {
      config = emacsConfig;
      package = pkgs.emacs;
      alwaysEnsure = true;
      override = epkgs: epkgs // {
        # org-roam = orgRoam;
      };
    });
    extraPackages = emacsPkgs: with emacsPkgs; [
      use-package

      # org-roam dependencies
      dash
      f
      s
      emacsql
      emacsql-sqlite
      magit-section
    ];
  };

  home.packages = with pkgs; [
    ripgrep
    fd
    clang
    sqlite # for org-roam

    plantuml
  ];

  home.file = {
    emacsConfig = {
      source = emacsConfig;
      target = ".emacs";
    };
  #   orgTransclusion = {
  #     source = orgTransclusion;
  #     target = ".emacs.d/org-transclusion/";
  #   };
  #   orgRoam = {
  #     source = orgRoam;
  #     target = ".emacs.d/org-roam/";
  #   };
  };
}
