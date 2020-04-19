{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    /home/jeyj0/nixos/hosts/jannis-arcusx-hardware.nix
    /home/jeyj0/nixos/common.nix
  ];

  # boot setup
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel ];
  hardware.acpilight.enable = true;
  hardware.brightnessctl.enable = true;

  services.illum.enable = true;

  networking.hostName = "jannis-arcusx";
  # networking.wireless = true;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;
}
