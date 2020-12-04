# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ fetchFromGitHub, config, lib, pkgs, ... }:

with lib;

let
  packages = import ./nix/packages.nix {};

  pythonPackages = pypkgs: with pypkgs; [
    pynvim
  ];
  pythonWithPackages = pkgs.python3.withPackages pythonPackages;
in
{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  system.autoUpgrade = {
    # enable = true;
    allowReboot = false;
    channel = https://nixos.org/channels/nixos-20.03;
  };

  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];

  # imports =
    # [ # Include the results of the hardware scan.
      # /etc/nixos/hardware-configuration.nix
    # ];

  hardware = {
    bluetooth.enable = true;
    opengl = {
      driSupport32Bit = true;
      # extraPackages = with pkgs; [ vaapiIntel ];
    };
  };

  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;

  # Use the GRUB 2 boot loader.
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.

  networking = {
    # hostName = "jannis-arcusx"; # Define your hostname.
    # wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    networkmanager.enable = true;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  # networking.interfaces.enp0s31f6.useDHCP = true;
  # networking.interfaces.enp0s31f6.useDHCP = true;
  # networking.interfaces.wlp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    groups = {
      jeyj0 = {
        members = [ "jeyj0" ];
      };
    };
    users.jeyj0 = {
      isNormalUser = true;
      extraGroups = [ "jeyj0" "wheel" "docker" "audio" "video" ]; # Enable ‘sudo’ for the user.
      shell = pkgs.fish;
      createHome = true;
      home = "/home/jeyj0";
      description = "Jannis Jorre";
    };
  };

  nix = {
    trustedUsers = [ "root" "jeyj0" ];
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  networking.firewall.enable = true;
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 80 443 4443 8000 3000 8080 8001 8081 8082 8083 8084 8085 5000 9993 10000 ];
  networking.firewall.allowedUDPPorts = [ 10000 9993 ];

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  location.provider = "geoclue2";
  services = {
    zerotierone.enable = true;
    syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = "jeyj0";
      group = "jeyj0";
      dataDir = "/home/jeyj0/Sync";
      configDir = "/home/jeyj0/.config/syncthing/";
    };
    printing.enable = true;
    emacs = {
      enable = true;
      install = true;
    };
    unclutter = {
      enable = true;
      keystroke = true;
      package = pkgs.unclutter-xfixes;
    };
    openssh.enable = true;
    lorri.enable = true;
    blueman.enable = true;
    redshift = {
      enable = true;
      temperature = {
        night = 3000;
      };
    };
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    # videoDrivers = [ "nvidia" ];

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
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        extraPackages = with pkgs; [
          dmenu
          rofi
          polybarFull
        ];
      };
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ../.xmonad/xmonad.hs;
        extraPackages = hpkgs: with hpkgs; [
          xmonad
          xmonad-contrib
          xmonad-extras
        ];
      };
    };
  };

  # services.clipmenu.enable = true;

  virtualisation.docker = {
    enable = true;
    # enableNvidia = true;
  };

  # system-wide program settings
  programs = {
    command-not-found.enable = true;
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

  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;
    packageOverrides = pkgs: {};
  };

  environment.variables = {
    TERMINAL = "alacritty";
    EDITOR = "nvim";
    VISUAL = "nvim";
  };
  environment.systemPackages = with packages; [
    # xmonad
    xmobar
    xdotool
    battery-status

    # nodejs
    nodejs-12_x
    nodePackages.eslint

    # emacs
      ripgrep
      fd
      clang
      sqlite # for org-roam

    vscodium

    rustup
    wasm-pack

    # basic utilities
    curl
    wget
    git
    gitAndTools.diff-so-fancy # pretty diffs
    gitAndTools.gh # github cli
    lazygit
    xclip
    trash-cli
    silver-searcher # ag (faster ack (faster & better grep))
    htop
    entr
    lsof
    direnv # for project-wise environments
    nitrogen # to set wallpapers
    compton # making windows fancy
    zip unzip
    tldr # tl;dr man page alternative
    docker-compose

    now-cli
    google-cloud-sdk
    kubectl

    # programs
    ## command line
    neovim-unwrapped
      watchman # watchman for coc.nvim
      neovim-remote # enable the use of only one neovim instance
      pythonWithPackages
    nnn
    ranger
      highlight # highlight enables syntax highlighting in previews
    fzf
    starship # beautiful command prompt
    xorg.xwininfo
    xorg.xmodmap
    networkmanager
    networkmanager_dmenu
    bat
    exa
    tuir
    cachix
    speedtest-cli
    fontpreview
    haskellPackages.hoogle
    jq
    plantuml
    texlive.combined.scheme-full

    slack
    discord
    spotify

    # minecraft # while this should work, the package is currently broken
    multimc

    # terminal emulators
    alacritty
    kitty

    ## other
    firefox
    chromium
    qutebrowser
    sxiv
    gimp

    apvlv
    evince

    obs-studio
    obs-v4l2sink
    ffmpeg
    vlc

    zoom-us

    nextcloud-client
    libreoffice
    postman
    openssl

    godot

    elmPackages.elm-language-server

    responsively

    zerotierone

    ## Games
    brogue

    cloudflared
  ];

  fonts.fonts = with packages; [
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

