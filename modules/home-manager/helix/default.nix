{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.helix = {
    enable = mkEnableOption "helix";
  };

  config = mkIf config.jeyj0.helix.enable {
    programs.helix = {
      enable = true;
      package = pkgs.jeyj0.helix;
      languages = [
        {
          name = "typescript";
          formatter = { command = "prettier"; args = ["--parser" "typescript"]; };
          auto-format = true;
        }
        {
          name = "javascript";
          formatter = { command = "prettier"; args = ["--parser" "javascript"]; };
          auto-format = true;
        }
        {
          name = "html";
          formatter = { command = "prettier"; args = ["--parser" "html"]; };
          auto-format = true;
        }
        {
          name = "json";
          formatter = { command = "prettier"; args = ["--parser" "json"]; };
          auto-format = true;
        }
        {
          name = "css";
          formatter = { command = "prettier"; args = ["--parser" "css"]; };
          auto-format = true;
        }
      ];
      settings = {
        theme = "tokyonight_storm";
        editor = {
          bufferline = "multiple";
          auto-format = true;
          auto-completion = true;
          completion-trigger-len = 1;
        };
        keys = {
          normal = {
            space = {
              space = "file_picker";
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

    home.packages = with pkgs.unstable; [
      # language servers
      nodePackages_latest.typescript-language-server
      nodePackages_latest.vscode-langservers-extracted
      nodePackages_latest.yaml-language-server
      nodePackages_latest.dockerfile-language-server-nodejs
      pkgs.jeyj0.nil # nix language server
      pkgs.jeyj0.node-packages."@prisma/language-server"
      taplo # TOML language server

      # tree sitter grammars
      tree-sitter-grammars.tree-sitter-nix
    ];
  };
}

