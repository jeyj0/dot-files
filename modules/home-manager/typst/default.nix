{ config
, pkgs
, lib
, ...
}:
with lib;
let
  # fonts = pkgs.stdenv.mkDerivation {
  #   name = "typst-ready-fonts";
  #   src = ./.;
  #   buildPhase = ''
  #     mkdir -p $out
  #     cp -r ${pkgs.unstable.cm_unicode} $out/
  #     cp -r ${pkgs.unstable.hack-font} $out/
  #   '';
  #   installPhase = '''';
  # };
in
{
  options.jeyj0.typst = {
    enable = mkEnableOption "typst";
  };

  config = mkIf config.jeyj0.typst.enable {
    home.packages = [ pkgs.jeyj0.typst ];
    # home.sessionVariables = {
    #   TYPST_FONT_PATHS = "${fonts}";
    # };
  };
}