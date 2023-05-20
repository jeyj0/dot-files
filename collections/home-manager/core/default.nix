{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.collections.core = {
    enable = mkEnableOption "collection-core";
  };

  config = mkIf config.jeyj0.collections.core.enable {
    jeyj0 = {
      git.enable = mkDefault true;
      fish.enable = mkDefault true;
      starship.enable = mkDefault true;
      neovim.enable = mkDefault true;
      exa.enable = mkDefault true;
      bat.enable = mkDefault true;
      trash.enable = mkDefault true;
      ag.enable = mkDefault true;
      clifm.enable = mkDefault true;
      htop.enable = mkDefault true;
      sshfs.enable = mkDefault true;
    };
  };
}

