{ winapps }:
{ config, pkgs, ... }:
let
in
{
  home.packages = [
    winapps
  ];

  home.file.winappsConfigs = {
    source = ./config/winapps;
    target = ".config/winapps";
    recursive = true;
  };
}
