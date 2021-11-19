# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ fetchFromGitHub, config, lib, pkgs, ... }:

with lib;

let
  userName = "jeyj0";
  userHome = "/home/${userName}";
  hostName = "jeyj0-nixos";

  nixPackages = ({ stdenv, nixStable, nixUnstable, lib }:
    stdenv.mkDerivation {
      inherit (nixUnstable) pname version;
      src = nixUnstable;
      installPhase = ''
        # Symlink-copy of nixUnstable
        cp -as $src $out
        chmod -R +rw $out
        cd $out/bin
        mv nix nixUnstable
        ln -s nixUnstable nixFlakes
        # All nix-* commands need to point at nixUnstable now (it's a multi-call binary)
        for cmd in nix-* ; do ln -sf nixUnstable "$cmd" ; done
        # Provide old nix as nixStable
        ln -s ${nixStable}/bin/nix nixStable
        # nix needs to be the old one for nix-bash-completions (and my muscle memory) to work
        ln -s nixStable nix
        cd -
        # Make nixUnstable's built-in bash-completions work for the new binary names
        cd $out/share/bash-completion/completions/
        cp nix nixUnstable
        cp nix nixFlakes
        # We need to replace the command names to be completed with the new ones
        substituteInPlace nixUnstable --replace " nix" " nixUnstable"
        substituteInPlace nixFlakes --replace " nix" " nixFlakes"
        # Don't try to complete the nix binary anymore, it's old Nix now and would conflict nix-bash-completions
        rm nix
        cd -
      '';

      meta = nixUnstable.meta // {
        description = "My custom nixStable nixUnstable hybrid";
        longDescription = ''
          My custom Nix with the `nix` command from nixStable and everything else from nixUnstable.
          The new `nix` is available via `nixUnstble` or `nixFlakes`. Bash completions work for these.
          Since `nix` is still the old one, nix-bash-completions also work just fine.
        '';
        maintainers = [ lib.maintainers.atemu ];
      };
    });
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
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ]; # used to build images for RPi

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
      "${userName}" = {
        members = [ "${userName}" ];
      };
    };
    users."${userName}" = {
      isNormalUser = true;
      extraGroups = [ userName "wheel" "docker" "lxd" "lxc" "audio" "video" ]; # Enable ‘sudo’ for the user.
      shell = pkgs.fish;
      createHome = true;
      home = userHome;
      description = "Jannis Jorre";
    };
  };

  nix = {
    trustedUsers = [ "root" "${userName}" ];
    package = pkgs.nixFlakes;
    autoOptimizeStore = true;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    binaryCaches = [
      "https://hydra.iohk.io" # haskell.nix
    ];
    trustedBinaryCaches = [
      "https://hydra.iohk.io" # haskell.nix
    ];
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # haskell.nix
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  networking.firewall.enable = true;
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 80 443 631 4443 8000 3000 8080 8001 8081 8082 8083 8084 8085 5000 9993 10000 ];
  networking.firewall.allowedUDPPorts = [ 10000 9993 631 ];

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  location.provider = "geoclue2";
  services = {
    printing = {
      enable = true;
      browsing = true;
      defaultShared = true;
      drivers = with pkgs; [
        hplip
      ];
    };
    avahi = {
      enable = true;
      nssmdns = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };
    zerotierone.enable = true;
    syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = "jeyj0";
      group = "jeyj0";
      dataDir = "/home/jeyj0/Sync";
      configDir = "/home/jeyj0/.config/syncthing/";
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
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ../.config/home-manager/parts/xmonad/xmonad.hs;
        extraPackages = hpkgs: with hpkgs; [
          xmonad
          xmonad-contrib
          xmonad-extras
        ];
      };
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
  };

  # system-wide program settings
  programs = {
    command-not-found.enable = true;
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
  nixpkgs.overlays = [
    (final: prev: {
      nix = pkgs.callPackage nixPackages {};
    })
  ];

  environment.variables = {
    TERMINAL = "alacritty";
    EDITOR = "nvim";
    VISUAL = "nvim";
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

