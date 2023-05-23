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
      languages = {
        language = let
          prettierFormatter = parser: { command = "prettier"; args = ["--parser" parser]; };
        in [
          {
            name = "typescript";
            formatter = prettierFormatter "typescript";
            language-servers = ["tailwindcss-typescript" "typescript-language-server"];
          }
          {
            name = "tsx";
            language-id = "typescriptreact";
            formatter = prettierFormatter "typescript";
            language-servers = ["tailwindcss-tsx" "typescript-language-server"];
          }
          {
            name = "javascript";
            formatter = prettierFormatter "javascript";
            language-servers = ["tailwindcss-javascript" "typescript-language-server"];
          }
          {
            name = "jsx";
            formatter = prettierFormatter "javascript";
            language-servers = ["tailwindcss-jsx" "typescript-language-server"];
          }
          {
            name = "html";
            formatter = prettierFormatter "html";
          }
          {
            name = "json";
            formatter = prettierFormatter "json";
          }
          {
            name = "css";
            scope = "source.css";
            file-types = ["css" "postcss"];
            formatter = prettierFormatter "css";
            language-servers = ["tailwindcss-css" "vscode-css-language-server"];
          }
        ];

        language-server = let
          tailwindLanguageServer = language-id: {
            language-id = "typescript";
            command = "hx-tw";
            args = ["--stdio"];
            roots = ["tailwind.config.js" "tailwind.config.cjs" ".prettierrc" "nx.json"];
          };
        in {
          tailwindcss-typescript = tailwindLanguageServer "typescript";
          tailwindcss-tsx = tailwindLanguageServer "typescriptreact";
          tailwindcss-javascript = tailwindLanguageServer "javascript";
          tailwindcss-jsx = tailwindLanguageServer "javascriptreact";
          tailwindcss-css = tailwindLanguageServer "css";
        };
      };
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
      nodePackages_latest.prettier

      # language servers
      nodePackages_latest.typescript-language-server
      nodePackages_latest.vscode-langservers-extracted
      nodePackages_latest.yaml-language-server
      nodePackages_latest.dockerfile-language-server-nodejs
      pkgs.jeyj0.nil # nix language server
      pkgs.jeyj0.node-packages."@prisma/language-server"
      pkgs.jeyj0.node-packages."@tailwindcss/language-server"
      pkgs.jeyj0.node-packages."helix-twcss"
      taplo # TOML language server
    ];
  };
}

