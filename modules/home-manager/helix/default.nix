{ config
, pkgs
, lib
, ...
}:
with lib;
let
  HELIX_RUNTIME = ".config/helix/runtime";
in
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
            auto-format = true;
            language-servers = ["typescript-language-server" "tailwindcss-typescript"];
          }
          {
            name = "tsx";
            language-id = "typescriptreact";
            formatter = prettierFormatter "typescript";
            auto-format = true;
            language-servers = [ "typescript-language-server" "tailwindcss-typescript"];
          }
          {
            name = "javascript";
            formatter = prettierFormatter "javascript";
            auto-format = true;
            language-servers = ["typescript-language-server" "tailwindcss-javascript"];
          }
          {
            name = "jsx";
            formatter = prettierFormatter "javascript";
            auto-format = true;
            language-servers = ["typescript-language-server" "tailwindcss-jsx"];
          }
          {
            name = "html";
            formatter = prettierFormatter "html";
            auto-format = true;
          }
          {
            name = "json";
            formatter = prettierFormatter "json";
            auto-format = true;
          }
          {
            name = "css";
            scope = "source.css";
            file-types = ["css" "postcss"];
            formatter = prettierFormatter "css";
            auto-format = true;
            language-servers = ["vscode-css-language-server" "tailwindcss-css"];
          }
          {
            name = "typst";
            scope = "source.typst";
            injection-regex = "typst";
            roots = [];
            comment-token = "//";
            file-types = ["typ"];
            indent = { tab-width = 2; unit = " "; };
            language-servers = ["typst-lsp"];
            text-width = 80;
            rulers = [80];
            soft-wrap.wrap-at-text-width = true;
          }
        ];

        grammar = [
          { name = "typst"; } # installation is done via home.file below
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
          typst-lsp = {
            language-id = "typst";
            command = "typst-lsp";
            args = ["--stdio"];
            roots = ["main.typ"];
          };
        };
      };
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

    home.file = {
      helix-typst-queries = {
        source = pkgs.helix-tree-sitter-typst + "/queries";
        target = "${HELIX_RUNTIME}/queries/typst";
        recursive = true;
      };
      helix-typst-grammar = {
        source = pkgs.helix-tree-sitter-typst + "/typst.so";
        target = "${HELIX_RUNTIME}/grammars/typst.so";
      };
    };

    home.sessionVariables = {
      inherit HELIX_RUNTIME;
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
      pkgs.jeyj0.typst-lsp
    ];
  };
}

