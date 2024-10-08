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
          id = "WQ7WJGO-OH4NAGL-WBTR6QN-W2JN2JC-3RQWJ5B-732PUZA-AF2BSND-EXLKLQG";
        };
        jeyj0-phone = {
          addresses = [];
          id = "3ZTBTG5-UWL5OCR-ALQBKM3-CXC3EDW-7T4IRIB-5RWQPST-GCVC54P-BEE3XQQ";
        };
        jeyj0-pixel8 = {
          addresses = [];
          id = "3O4WTCP-6VDSTQZ-OFM2W5E-K4NMN2I-QJFA7XT-V7AY5VG-UWOH2DN-MX7RCQD";
        };
        ronja-phone = {
          addresses = [];
          id = "RVI4BEL-YCCDBCR-J77I5NJ-2EK2SB4-3Q3CX7R-MJ3YDL5-7RMR5PB-GLY2OAV";
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
