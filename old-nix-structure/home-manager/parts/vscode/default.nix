{ pkgs }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.unstable.vscode;
    extensions = pkgs.unstable.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "language-haskell";
        publisher = "justusadam";
        version = "3.6.0";
        sha256 = "sha256-rZXRzPmu7IYmyRWANtpJp3wp0r/RwB7eGHEJa7hBvoQ=";
      }
      {
        name = "org-mode";
        publisher = "vscode-org-mode";
        version = "1.0.0";
        sha256 = "sha256-o9CIjMlYQQVRdtTlOp9BAVjqrfFIhhdvzlyhlcOv5rY=";
      }
      {
        name = "vscode-direnv";
        publisher = "cab404";
        version = "1.0.0";
        sha256 = "sha256-+nLH+T9v6TQCqKZw6HPN/ZevQ65FVm2SAo2V9RecM3Y=";
      }
      {
        name = "nix-env-selector";
        publisher = "arrterian";
        version = "1.0.9";
        sha256 = "sha256-DnaIXJ27bcpOrIp1hm7DcrlIzGSjo4RTJ9fD72ukKlc=";
      }
      {
        name = "nix-ide";
        publisher = "jnoortheen";
        version = "0.1.20";
        sha256 = "sha256-Q6X41I68m0jaCXaQGEFOoAbSUrr/wFhfCH5KrduOtZo=";
      }
      {
        name = "vscode-hsx";
        publisher = "s0kil";
        version = "0.4.0";
        sha256 = "sha256-K2L9GrFbXu/AxdgToAuaW9PXRuUgUx9rHd7MqimFUT8=";
      }
      {
        name = "haskell";
        publisher = "haskell";
        version = "2.2.0";
        sha256 = "sha256-dmfOS3KIaLsMl+aO+BSBwthVIAyDJRtPLPjcVzqdKOE=";
      }
      {
        name = "prettier-vscode";
        publisher = "esbenp";
        version = "9.5.0";
        sha256 = "sha256-L/jW6xAnJ8v9Qq+iyQI8usGr8BoICR+2ENAMGQ05r0A=";
      }
      {
        name = "copilot";
        publisher = "GitHub";
        version = "1.20.5902";
        sha256 = "sha256-jr6WfOB+efK8goNU9O0bIrcaerH+DxKkt4ioOaPpXh0=";
      }
      {
        name = "vscode-pull-request-github";
        publisher = "GitHub";
        version = "0.43.2022051220";
        sha256 = "sha256-Rj3xyMkJosCTeBN9rA4zL0EVhtorKYfqbTPiSgC8zbQ=";
      }
      {
        name = "gruvbox";
        publisher = "jdinhlife";
        version = "1.5.1";
        sha256 = "sha256-0ghB0E+Wa9W2bNFFiH2Q3pUJ9HV5+JfKohX4cRyevC8=";
      }
      {
        name = "vscode-docker";
        publisher = "ms-azuretools";
        version = "1.22.0";
        sha256 = "sha256-VtGqoSDagmYjq9vXtgluiOMB40WphDTWd0F3Y3mgNIs=";
      }
      {
        name = "project-manager";
        publisher = "alefragnani";
        version = "12.5.0";
        sha256 = "sha256-uIV9K67sBp3PRjd63Wycjfawxx0RZfkvl2NQj/lkX2w=";
      }
      {
        name = "sass-indented";
        publisher = "syler";
        version = "1.8.19";
        sha256 = "sha256-CHv4MbcqGqoJOkw4haR2jW8yl3PLiJKQ0OnISEutEhY=";
      }
    ];
    userSettings = {
      "editor.fontSize" = 16;
      "debug.console.fontSize" = 16;
      "markdown.preview.fontSize" = 16;
      "terminal.integrated.fontSize" = 16;
      "git.confirmSync" = false;
      "workbench.colorTheme" = "Gruvbox Dark Hard";
      "update.mode" = "none";
      "window.menuBarVisibility" = "toggle";
      "editor.minimap.enabled" = false;
      "[typescriptreact]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "[typescript]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "editor.formatOnSave" = true;
      "[javascript]" =  {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "[nix]" = {
        "editor.defaultFormatter" = "jnoortheen.nix-ide";
        "editor.formatOnSave" = false;
      };
      "terminal.integrated.defaultLocation" = "editor";
      "editor.fontFamily" = "'Hack Nerd Font', 'Hack', 'monospace', monospace";
      "editor.inlineSuggest.enabled" = true;
      "projectManager.git.baseFolders" = [
        "~/projects"
      ];
      "projectManager.git.maxDepthRecursion" = 1;
      "workbench.startupEditor" = "none";
      "projectManager.projectsLocation" = "~/.config/home-manager/parts/vscode/"; # TODO remove this
    };
    keybindings = [
      # {
      #   key = "ctrl+c";
      #   command = "editor.action.clipboardCopyAction";
      #   when = "textInputFocus";
      # }
    ];
  };

  home.file = {
    projectManagerConfig = {
      source = ./projects.json;
      target = ".config/Code/User/globalStorage/alefragnani.project-manager/projects.json";
    };
  };
}