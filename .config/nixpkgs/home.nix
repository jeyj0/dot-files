{ config, pkgs, ... }:
let
  mac-config = import ../home-manager/mac.nix {
    userName = "digitallyinduced";
    userHome = "/Users/digitallyinduced";
  } { pkgs = pkgs; };
in
{
  imports = [
    mac-config
  ];
}
