{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.user-jeyj0 = {
    enable = mkEnableOption "user-jeyj0";
  };

  config = mkIf config.jeyj0.user-jeyj0.enable {
    # Define a user account. Don't forget to set a password with ‘passwd’.
    users = {
      groups = {
        jeyj0 = {
          members = [ "jeyj0" ];
        };
      };
      users.jeyj0 = {
        isNormalUser = true;
        description = "Jannis Jorre";
        extraGroups = [ "jeyj0" "wheel" "docker" "lxd" "lxc" "audio" "video" ];
        shell = pkgs.unstable.fish;
        createHome = true;
        home = "/home/jeyj0";
      };
    };
  };
}