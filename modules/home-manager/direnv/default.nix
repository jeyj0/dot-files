{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.direnv = {
    enable = mkEnableOption "direnv";
  };

  config = mkIf config.jeyj0.direnv.enable {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
      nix-direnv.enableFlakes = true;
    };
  };
}

