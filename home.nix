{ config, pkgs, ... }:
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "jeyj0";
  home.homeDirectory = "/var/home/jeyj0";

  jeyj0 = {
    hostName = "jeyj0-framework";
  };

  imports = [
    ./jeyj0.nix
    ./modules/gtk-theme.nix
    ./modules/git.nix
    ./modules/syncthing.nix
  ];

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.sessionVariables = {
    # add environment variables here
  };

  home.packages = with pkgs; [
    # packages that don't have a programs module
    trash-cli
  ];

  programs.eza.enable = true;
  programs.bat.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      source (/usr/bin/starship init fish --print-full-init | psub)
    '';
    shellAliases = {
      # general
      cp = "cp -i";
      mv = "mv -i";
      tree = "eza --tree";
      op = "cd ~/projects/(ls ~/projects/ | fzf)";

      # git
      gs = "git status";
      gss = "git status -s";
      gpush = "git push";
      gpull = "git pull";
      gc = "git commit";
      gco = "git_checkout_fzf";
      glog = "git log";
      # glog-graph = "git log --graph --abbrev-commit --color=always --decorate --format=format:\"\%C(blue)\%h\%C(reset) - \%C(white bold)\%s\%C(reset) \%C(dim white)- \%an\%C(reset)\%n""           \%C(cyan)\%aD\%C(reset) \%C(green)(\%ar)\%C(reset)\%C(auto)\%d\%C(reset)\"";
      ga = "git add";
      gd = "git diff";
      gdc = "git diff --cached";

      # make deletion of files on command line safer
      rm = "trash";

      # alias e to the default editor
      e = "$VISUAL";

      # utilities
      whatismyip = "curl ipinfo.io/ip";

      # managing the windows vm for winapps
      windows-create = "docker compose --file ~/.config/winapps/compose.yaml up -d"; # Create a new Windows VM (assuming none exists yet)
      windows-destroy = "docker compose --file ~/.config/winapps/compose.yaml down"; # Destroy the Windows VM container (data is preserved)
      windows-start = "docker compose --file ~/.config/winapps/compose.yaml start"; # Power on the Windows VM
      windows-pause = "docker compose --file ~/.config/winapps/compose.yaml pause"; # Pause the Windows VM
      windows-unpause = "docker compose --file ~/.config/winapps/compose.yaml unpause"; # Resume the Windows VM
      windows-restart = "docker compose --file ~/.config/winapps/compose.yaml restart"; # Restart the Windows VM
      windows-stop = "docker compose --file ~/.config/winapps/compose.yaml stop"; # Gracefully shut down the Windows VM
      windows-kill = "docker compose --file ~/.config/winapps/compose.yaml kill"; # Force shut down the Windows VM
    };
  };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      add_newline = false;
      directory = {
        truncation_length = 1;
      };
      nix_shell = {
        disabled = true;
      };
      gcloud = {
        disabled = true;
      };
    };
  };

  programs.helix = {
    enable = true;
    defaultEditor = true;
    settings = {
      theme = "tokyonight_storm";
      editor = {
        bufferline = "multiple";
        auto-format = true;
        auto-completion = true;
        completion-trigger-len = 1;
        gutters = ["diff" "diagnostics" "spacer" "line-numbers" "spacer"];
        idle-timeout = 0;
        color-modes = true;
        indent-guides.render = true;

        lsp.goto-reference-include-declaration = false;
      };
      keys = {
        select = {
          "}" = "goto_next_paragraph";
          "{" = "goto_prev_paragraph";
        };
        normal = {
          "}" = "goto_next_paragraph";
          "{" = "goto_prev_paragraph";
          space = {
            space = "file_picker";
            "." = "code_action";
            c = "toggle_comments";
            f = {
              s = ":write";
            };
            b = {
              b = "buffer_picker";
              n = ":buffer-next";
              p = ":buffer-previous";
              d = ":buffer-close";
              D = ":buffer-close!";
              O = ":buffer-close-others";
            };
          };
        };
        insert = {
          "C-space" = "completion";
        };
      };
    };
  };

  home.file.vscode-settings = {
    target = ".config/Code/User/settings.json";
    text = ''
      {
        "window.titleBarStyle": "custom",
        "editor.fontFamily": "'Cascadia Code', 'Droid Sans Mono', 'monospace', monospace",
        "window.zoomLevel": 2,
        "workbench.preferredDarkColorTheme": "Tokyo Night Storm",
        "workbench.preferredLightColorTheme": "Tokyo Night Light",
        "window.autoDetectColorScheme": true,
        "workbench.chat.experimental.showWelcomeView": false,
        "git.confirmSync": false,
        "git.autofetch": true,
        "editor.minimap.enabled": false,
        "editor.formatOnSave": true,
        "workbench.startupEditor": "none",
        "[typescript]": {
          "editor.defaultFormatter": "esbenp.prettier-vscode"
        },
        "[html]": {
          "editor.defaultFormatter": "esbenp.prettier-vscode"
        },
        "[typescriptreact]": {
          "editor.defaultFormatter": "esbenp.prettier-vscode"
        }
      }
    '';
  };
}
