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
      nix-index.enable = mkDefault true; # command-not-found replacement
      git.enable = mkDefault true;
      home-manager.enable = mkDefault true;
      fish.enable = mkDefault true;
      starship.enable = mkDefault true;
      helix.enable = mkDefault true;
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

