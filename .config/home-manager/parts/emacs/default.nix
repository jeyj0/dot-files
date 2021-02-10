{ pkgs, installEmacs ? true }:
{
  programs.emacs = {
    enable = true;#installEmacs;
    package = pkgs.emacs;
    extraPackages = emacsPkgs: with emacsPkgs; [
      evil
      evil-surround
      ivy

      org # can't simply 
      org-roam
      visual-fill-column

      doom-themes

      #lsp-mode
      #tree-sitter
    ];
  };

  home.packages = with pkgs; [
    ripgrep
    fd
    clang
    sqlite # for org-roam

    plantuml
  ];# ++ (if installEmacs then [ pkgs.emacs ] else []);
}
