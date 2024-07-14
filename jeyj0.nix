{ lib
, ...
}:
with lib;
{
  options.jeyj0 = {
    enable = mkEnableOption "jeyj0";
    hostName = mkOption {
      type = types.str;
      description = ''
        The hostname this home configuration is made for.
      '';
    };
  };
}
