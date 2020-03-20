# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

with lib;

let
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  hardware = {
    bluetooth.enable = true;
    opengl.driSupport32Bit = true;
  };

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sdb"; # or "nodev" for efi only

  networking.hostName = "jeyj0-nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Fira-Code";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jeyj0 = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
    createHome = true;
    home = "/home/jeyj0";
    description = "Jannis Jorre";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services = {
    openssh.enable = true;
    lorri.enable = true;
    blueman.enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    videoDrivers = [ "nvidia" ];

    desktopManager = {
      xterm.enable = false;
      xfce4-14 = {
        enable = true;
        enableXfwm = false;
        noDesktop = true;
      };
    };

    windowManager = {
      default = "i3";
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [
          dmenu
          i3status
          i3lock
          i3blocks
        ];
      };
    };
  };

  # services.clipmenu.enable = true;

  virtualisation.docker = {
    enable = true;
    enableNvidia = true;
  };

  # system-wide program settings
  programs = {
    fish = {
      enable = true;
    };
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
      interactiveShellInit = ''
        export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/
      '';
      promptInit = "";
      ohMyZsh = {
        enable = true;
      };
    };
  };

  # globally installed packages

  # add unstable derivative, so I can install unstable packages
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  environment.variables = {
    TERMINAL = "kitty";
    EDITOR = "nvim";
    VISUAL = "emacs";
  };
  environment.systemPackages = with pkgs; [
    # nodejs
    unstable.nodejs-13_x

    emacs
      ripgrep
      fd
      clang

    rustup
    wasm-pack

    # basic utilities
    curl
    wget
    git
    xclip
    trash-cli
    silver-searcher # ag (faster ack (faster & better grep))
    htop
    entr
    direnv # for project-wise environments

    # programs
    ## command line
    unstable.neovim-unwrapped
      watchman # watchman for coc.nvim
      neovim-remote # enable the use of only one neovim instance
    ranger
      highlight # highlight enables syntax highlighting in previews
    fzf
    starship # beautiful command prompt

    slack
    unstable.discord

    # minecraft # while this should work, the package is currently broken
    multimc

    # terminal emulators
    kitty

    ## other
    firefox
    unstable.qutebrowser
    sxiv
  ];

  fonts = {
    fonts = with pkgs; [
      fira-code
      hack-font
    ];
  };

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;
}

