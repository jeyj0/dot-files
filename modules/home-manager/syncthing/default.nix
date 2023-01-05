{ config
, pkgs
, lib
, ...
}:
with lib;
let
  folderModule = { id, path, stignore }: { config, ... }: {
    options.jeyj0.syncthing.folders.${id} = mkEnableOption id;
    config = let conf = config.jeyj0.syncthing; in mkIf conf.enable {
      home.file = {
        "${id}/.stignore" = mkIf conf.folders.${id} {
          target = "${path}/.stignore";
          text = stignore;
        };
      };
    };
  };
in
{
  imports = [
    (folderModule {
      id = "02-areas/00-ttrpgs";
      path = "Documents/02-areas/00-ttrpgs";
      stignore = ''
        .obsidian
      '';
    })
    (folderModule {
      id = "02-areas";
      path = "Documents/02-areas";
      stignore = ''
        /00-ttrpgs
      '';
    })
    (folderModule {
      id = "666-test";
      path = "Documents/666-test";
      stignore = ''
        .obsidian
      '';
    })
  ];

  options.jeyj0.syncthing = {
    enable = mkEnableOption "syncthing";
  };
}