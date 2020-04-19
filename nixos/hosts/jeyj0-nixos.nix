{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    /home/jeyj0/nixos/hosts/jeyj0-nixos-hardware.nix
    /home/jeyj0/nixos/common.nix
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sdb";

  networking.hostName = "jeyj0-nixos";

  networking.interfactes.enp0s31f6.useDHCP = true;

  services.xserver.videoDrivers = [ "nvidia" ];

  virtualisation.docker.enableNvidia = true;
}
