inputs@{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.unmigrated-framework-config = {
    enable = mkEnableOption "unmigrated-framework-config";
  };

  config = mkIf config.jeyj0.unmigrated-framework-config.enable (import ./configuration.nix inputs);
}
