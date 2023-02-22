{ config
, pkgs
, lib
, ...
}:
with lib;
let
  prettier = pkgs.unstable.writeShellScriptBin "prettier" ''
    ${pkgs.unstable.nodePackages_latest.prettier}/lib/node_modules/prettier/bin-prettier.js $@
  '';
in
{
  options.jeyj0.neovim = {
    enable = mkEnableOption "neovim";
  };

  config = mkIf config.jeyj0.neovim.enable {
    home.packages = with pkgs.unstable; [
      pkgs.jeyj0.neovim
      prettier # for auto-formatting
      ripgrep
    ];
    home.file.neovimConfigs = {
      source = ./config;
      target = ".config/nvim";
      recursive = true;
    };
  };
}
