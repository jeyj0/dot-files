{ config
, pkgs
, lib
, ...
}:
with lib;
let
  folderModule = { id, path, stignore, devices }: { config, ... }: {
    config = {
      home.file = {
        "${id}/.stignore" = mkIf (elem config.jeyj0.hostName devices) {
          target = "${path}/.stignore";
          text = stignore;  
        };
      };
    };
  };
  syncthingFolders = import ../syncthing-folders.nix;
  formatFolder = { id, label, path, stignore, devices }: {
    inherit id stignore devices;
    path = "Documents/${path}";
  };
in
{
  imports = map folderModule (map formatFolder syncthingFolders);

  services.syncthing = {
    enable = true;
  };
}
