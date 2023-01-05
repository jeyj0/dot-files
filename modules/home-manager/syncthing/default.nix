{ config
, pkgs
, lib
, ...
}:
with lib;
let
  folderModule = { id, path, stignore, devices }: { config, ... }: {
    config = let conf = config.jeyj0.syncthing; in mkIf conf.enable {
      home.file = {
        "${id}/.stignore" = mkIf (elem config.jeyj0.hostName devices) {
          target = "${path}/.stignore";
          text = stignore;
        };
      };
    };
  };
  syncthingFolders = import ../../../syncthing-folders.nix;
  formatFolder = { id, label, path, stignore, devices }: {
    inherit id stignore devices;
    path = "Documents/${path}";
  };
in
{
  imports = map folderModule (map formatFolder syncthingFolders);

  options.jeyj0.syncthing = {
    enable = mkEnableOption "syncthing";
  };
}
