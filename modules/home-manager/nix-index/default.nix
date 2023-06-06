inputs: { config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.nix-index = {
    enable = mkEnableOption "nix-index";
  };

  # TODO I'd like this to depend on my nix-index module being enabled
  imports = [inputs.nix-index-database.hmModules.nix-index];

  config = mkIf config.jeyj0.nix-index.enable {
    programs.nix-index = {
      enable = true;
      # package = pkgs.unstable.nix-index;
      enableBashIntegration = true; # I assume that I'll always have bash installed
      enableFishIntegration = mkIf config.jeyj0.fish.enable true;
    };
  };
}