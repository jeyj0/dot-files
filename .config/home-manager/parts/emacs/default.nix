{ pkgs }:
let
  emacsConfig = ./emacs.el;
  orgTransclusion = pkgs.fetchFromGitHub {
    owner = "nobiot";
    repo = "org-transclusion";
    rev = "c1b20436911f364c9477a17905af2881c765da14";
    sha256 = "0plb0wj461qb4crfbk6lsl45ixj0fz6xddcv46n5fs829d901hds";
  };
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

  home.file = {
    emacsConfig = {
      source = emacsConfig;
      target = ".emacs";
    };
    orgTransclusion = {
      source = orgTransclusion;
      target = ".emacs.d/org-transclusion/";
    };
  };
}
