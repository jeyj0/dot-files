{ pkgs, installEmacs ? true }:
{
  programs.emacs = {
    enable = true;#installEmacs;
    package = pkgs.emacs;
    extraPackages = emacsPkgs: with emacsPkgs; [
      evil
      org # can't simply 
      org-roam
      ivy
      visual-fill-column

      gruvbox-theme

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
