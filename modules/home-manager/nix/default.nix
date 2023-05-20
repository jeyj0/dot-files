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
        substituters = "https://cache.nixos.org https://cache.nixos.org/ https://digitallyinduced.cachix.org https://nix-community.cachix.org";
        trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
      };
    };
  };
}
