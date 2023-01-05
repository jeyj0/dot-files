{ config
, pkgs
, lib
, ...
}:
with lib;
let
  folderModule = { label, id, path }: { config, ...}: {
      options.jeyj0.syncthing.folders.${id} = mkEnableOption id;
      config = let conf = config.jeyj0.syncthing; in mkIf conf.enable {
        services.syncthing.folders.${id} = mkIf conf.folders.${id} {
          inherit id label;
          path = "/home/jeyj0/Documents/${path}";
        };
      };
    };
in
{
  imports = [
    (folderModule {
      label = "00-ttrpgs";
      id = "02-areas/00-ttrpgs";
      path = "02-areas/00-ttrpgs";
    })
    (folderModule {
      label = "02-areas";
      id = "02-areas";
      path = "02-areas";
    })
    (folderModule {
      label = "666-test";
      id = "666-test";
      path = "666-test";
    })
  ];

  options.jeyj0.syncthing = {
    enable = mkEnableOption "syncthing";
  };

  config = let conf = config.jeyj0.syncthing; in mkIf conf.enable {
    services.syncthing = {
      enable = true;
      package = pkgs.unstable.syncthing;

      systemService = true;

      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8384";

      overrideDevices = true;
      devices = {
        jeyj0-nixos = {
          addresses = [];
          id = "XQAR7J2-CQT2Y6W-7LV3WLB-YZLHOS5-DNDNIVJ-TWJS6CK-ZUEXPXT-G4WDZAP";
        };
      };

      overrideFolders = true;

      user = "jeyj0";
      group = "jeyj0";
      dataDir = "/home/jeyj0/Documents";
      configDir = "/home/jeyj0/.config/syncthing";
    };
  };
}