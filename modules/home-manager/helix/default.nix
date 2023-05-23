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
          language-servers = with pkgs.nodePackages_latest; [
            {
              command = "${typescript-language-server}/bin/typescript-language-server";
              args = [ "--stdio" "--tsserver-path=${typescript}/lib/node_modules/typescript/lib" ];
            }
          ];
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
        keys.normal = {
          space = {
            space = "file_picker";
            f = {
              s = ":write";
            };
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

      # tree sitter grammars
      tree-sitter-grammars.tree-sitter-nix
    ];
  };
}

