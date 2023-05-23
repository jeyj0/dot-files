{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.neovim = {
    enable = mkEnableOption "neovim";
  };

  config = mkIf config.jeyj0.neovim.enable {
    home.packages = with pkgs.unstable; [
      pkgs.jeyj0.neovim
      # prettier # for auto-formatting
      nodePackages_latest.prettier
      ripgrep
    ];
    home.file.neovimConfigs = {
      source = ./config;
      target = ".config/nvim";
      recursive = true;
    };
  };
}
