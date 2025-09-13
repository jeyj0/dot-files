{ config, lib, pkgs, ... }:
{
  home.stateVersion = "24.05";
  home.username = "jeyj0";
  home.homeDirectory = "/home/jeyj0";

  nix = {
    package = pkgs.nix;
    settings = {
      extra-experimental-features = ["nix-command" "flakes"];
    };
  };

  programs.home-manager.enable = true;
  programs.eza.enable = true;
  programs.bat.enable = true;
  programs.zoxide = {
    enable = true;
    options = ["--cmd cd"];
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      cp = "cp -i";
      mv = "mv -i";
      tree = "eza --tree";

      gs = "git status";
      gss = "git status -s";
      gc = "git commit";
      ga = "git add";
      gd = "git diff";
      gdc = "git diff --cached";
    };
  };

  programs.zellij = {
    enable = true;
    enableFishIntegration = true;
    exitShellOnExit = true;
    settings = {
      show_startup_tips = false;
      pane_frames = false;
      ui = {
        pane_frames = {
          hide_session_name = true;
        };
      };
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
      nix_shell.disabled = true;
      battery.display = [{
        threshold = 100;
      }];
    };
  };

  programs.helix = {
    enable = true;
    defaultEditor = true;
    settings = {
      theme = "tokyonight";
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
}
