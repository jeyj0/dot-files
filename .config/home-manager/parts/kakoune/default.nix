{ pkgs }:
{
  programs.kakoune = {
    enable = true;
    config = {
      colorScheme = "gruvbox";
      autoReload = "ask";
      autoComplete = [ "insert" "prompt" ];
      numberLines = {
        enable = true;
        highlightCursor = true;
        relative = true;
        separator = "|";
      };
      showMatching = true;
      ui = {
        assistant = "none";
      };
      wrapLines = {
        # enable = true;
        indent = true;
        word = true;
        # maxWidth = 80;
      };
    };
  };

  # this is a test of what is looks like when I type a lot and exceed the line limit. It should just wrap the line. And it does! That's pretty cool. And this is interesting with which the line is displayed is cool too. Now let's see whether or not the navigation still works reasonably...

  home.packages = with pkgs; [
    kak-lsp
  ];
}
