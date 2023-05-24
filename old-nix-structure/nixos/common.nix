# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

with lib;

let
  userName = "jeyj0";
  userHome = "/home/${userName}";
  hostName = "jeyj0-nixos";
in
{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ]; # used to build images for RPi

  # imports =
    # [ # Include the results of the hardware scan.
      # /etc/nixos/hardware-configuration.nix
    # ];

  hardware = {
    bluetooth.enable = true;
    opengl = {
      enable = true;
      driSupport32Bit = true;
      # extraPackages = with pkgs; [ vaapiIntel ];
    };
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  networking = {
    # hostName = "jannis-arcusx"; # Define your hostname.
    # wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    networkmanager.enable = true;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    extraGroups.vboxusers.members = [ "${userName}" ];
    groups = {
      "${userName}" = {
        members = [ "${userName}" ];
      };
    };
    users."${userName}" = {
      isNormalUser = true;
      extraGroups = [ userName "wheel" "docker" "lxd" "lxc" "audio" "video" ]; # Enable ‘sudo’ for the user.
      shell = pkgs.unstable.fish;
      createHome = true;
      home = userHome;
      description = "Jannis Jorre";
    };
  };

  nix = {
    settings = {
      trusted-users = [ "root" "${userName}" ];
      auto-optimise-store = true;
    };
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

  networking.firewall.enable = true;
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 80 443 631 4443 8000 3000 8080 8001 8081 8082 8083 8084 8085 5000 9993 10000 ];
  networking.firewall.allowedUDPPorts = [ 10000 9993 631 ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  location.provider = "geoclue2";
  services = {
    
    avahi = {
      enable = true;
      nssmdns = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };
    # syncthing = {
    #   enable = true;
    #   openDefaultPorts = true;
    #   user = "jeyj0";
    #   group = "jeyj0";
    #   dataDir = "/home/jeyj0/Sync";
    #   configDir = "/home/jeyj0/.config/syncthing/";
    # };
    unclutter = {
      enable = true;
      keystroke = true;
      package = pkgs.unclutter-xfixes;
    };
    openssh.enable = true;
    blueman.enable = true;
    redshift = {
      enable = true;
      temperature = {
        night = 3000;
      };
    };
    gnome.gnome-keyring.enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    videoDrivers = [ "nvidia" ];

    displayManager.defaultSession = "xfce+xmonad";

    desktopManager = {
      xterm.enable = false;
      xfce = {
        enable = true;
        enableXfwm = false;
        noDesktop = true;
      };
    };

    windowManager = {
      session = [{
        name = "xmonad";
        start = ''
           systemd-cat -t xmonad -- ${pkgs.xmonad}/bin/xmonad ${lib.escapeShellArgs []} &
           waitPID=$!
        '';
      }];
      # xmonad = {
      #   enable = true;
      #   # enableContribAndExtras = true;
      #   # config = ../home-manager/parts/xmonad/xmonad.hs;
      #   # extraPackages = hpkgs: with hpkgs; [
      #   #   xmonad
      #   #   xmonad-contrib
      #   #   xmonad-extras
      #   #   dbus
      #   #   monad-logger
      #   # ];
      # };
    };
  };

  # services.clipmenu.enable = true;

  virtualisation = {
    lxc.enable = true;
    lxd.enable = true;
    docker = {
      enable = true;
      # enableNvidia = true;
    };
    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };

  # system-wide program settings
  programs = {
    command-not-found.enable = true;
  };

  # globally installed packages

  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;
    packageOverrides = pkgs: {};
  };

  environment.variables = {
    TERMINAL = "alacritty";
    # EDITOR = "nvim";
    # VISUAL = "nvim";
  };

  fonts.fonts = with pkgs; [
    fira-code
    hack-font
    font-awesome
    open-sans
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "Hack"
        "OpenDyslexic"
      ];
    })
    dnd-fonts
  ];

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;
}

