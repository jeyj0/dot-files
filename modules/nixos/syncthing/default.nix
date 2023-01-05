{ config
, pkgs
, lib
, ...
}:
with lib;
let
  folderModule = { label, id, path, devices }@folder: { config, ...}: {
      config = let conf = config.jeyj0.syncthing; in mkIf conf.enable {
        services.syncthing.folders.${id} =
          let shouldAddFolder = elem config.networking.hostName devices;
          in mkIf shouldAddFolder folder;
      };
    };
  syncthingFolders = import ../../../syncthing-folders.nix;
  formatFolder = { id, label, path, stignore, devices }: {
    inherit id label devices;
    path = "/home/jeyj0/Documents/${path}";
  };
in
{
  imports = map folderModule (map formatFolder syncthingFolders);

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
        jeyj0-framework = {
          addresses = [];
          id = "Q6CMJEV-PF3LYJU-XIOB6N3-2EQDW6O-GMGUMLQ-VBXW6W5-LRGZQHR-O6LV5AK";
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
