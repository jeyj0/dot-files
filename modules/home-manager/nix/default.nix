{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.nix = {
    enable = mkEnableOption "nix";
  };

  config = mkIf config.jeyj0.nix.enable {
    nix = {
      package = pkgs.unstable.nix;
      settings = {
        experimental-features = "nix-command flakes";
      };
    };
  };
}