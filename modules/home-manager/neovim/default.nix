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
    home.packages = with pkgs.unstable; [ neovim ];
    # programs.neovim = {
    #   enable = true;
    #   package = pkgs.neovim;
    # };
    home.file.neovimConfigs = {
      source = ./xdg-config;
      target = ".config/nvim";
      recursive = true;
    };
  };
}
