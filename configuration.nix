{ config, pkgs, lib, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "jeyj0-minimal"; # Define your hostname.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "de_DE.UTF-8";
    LC_IDENTIFICATION = "de_DE.UTF-8";
    LC_MEASUREMENT = "de_DE.UTF-8";
    LC_MONETARY = "de_DE.UTF-8";
    LC_NAME = "de_DE.UTF-8";
    LC_NUMERIC = "de_DE.UTF-8";
    LC_PAPER = "de_DE.UTF-8";
    LC_TELEPHONE = "de_DE.UTF-8";
    LC_TIME = "de_DE.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jeyj0 = {
    isNormalUser = true;
    description = "Jannis Jorre";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
    shell = pkgs.fish;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.sessionVariables = {
    COLORTERM = "truecolor";
  };

  environment.systemPackages = with pkgs; [
    helix
    zellij
    git
  ];

  programs.fish.enable = true;

  services.kmscon = {
    enable = true;
    useXkbConfig = true;
    fonts = [{name = "Jetbrains Mono Nerd Font"; package = pkgs.nerd-fonts.jetbrains-mono;}];
    extraOptions = lib.concatStringsSep " " [
      "--font-size 18"
      "--palette custom"
      # unless specific otherwise, colors taken from tokyonight alacritty theme
      "--palette-black 21,22,30"
      "--palette-red 247,118,142"
      "--palette-green 158,206,106"
      "--palette-yellow 224,175,104"
      "--palette-blue 122,162,247"
      "--palette-magenta 187,154,247"
      "--palette-cyan 125,207,255"
      "--palette-light-grey 115,122,162" # helix tokyonight fg-linenr
      "--palette-dark-grey 59,66,97" # helix tokyonight fg-gutter
      "--palette-light-red 255,137,157"
      "--palette-light-green 159,224,68"
      "--palette-light-yellow 250,186,74"
      "--palette-light-blue 141,176,255"
      "--palette-light-magenta 199,169,255"
      "--palette-light-cyan 164,218,255"
      "--palette-white 192,202,245" # tokynight alacritty bright white
      "--palette-foreground 192,202,245"
      "--palette-background 26,27,38"
    ];
    package = pkgs.kmscon.overrideAttrs ({
      mesonFlags = [ "-Dbackspace_sends_delete=true" ];
    });
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
