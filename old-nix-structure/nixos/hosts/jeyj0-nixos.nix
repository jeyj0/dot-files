{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ./jeyj0-nixos-hardware.nix
    ../common.nix
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sdb";

  networking.hostName = "jeyj0-nixos";

  networking.interfaces.enp0s31f6.useDHCP = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  services.udev.packages = [ pkgs.unstable.qmk-udev-rules ];

  virtualisation.docker.enableNvidia = true;

  fileSystems."/home/jeyj0/Documents/06-mass-storage" =
    { device = "/dev/disk/by-uuid/f63ce1a5-b76e-46a9-a1fc-c7afa558d391";
      options = [
        "defaults"
      ];
      fsType = "ext4";
    };
}
