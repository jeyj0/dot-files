{ config, pkgs, ... }:
let
  userName = "jeyj0";
  userHome = "/Users/${userName}";
  mac-config = import ../home-manager/mac.nix {
    userName = userName;
    userHome = userHome;
  } { pkgs = pkgs; };
in
{
  imports = [
    mac-config
  ];
}
