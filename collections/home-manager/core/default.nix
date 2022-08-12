{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.collections.core = {
    enable = mkEnableOption "collection-core";
  };

  config = mkIf config.jeyj0.collections.core.enable {
    jeyj0 = {
      git.enable = true;
      fish.enable = true;
      starship.enable = true;
    };
  };
}

