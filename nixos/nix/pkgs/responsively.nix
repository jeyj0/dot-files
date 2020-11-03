{ stdenv, fetchurl, appimageTools }:

let
  inherit (appimageTools) wrapType2;

  name = "responsively";
  src = fetchurl {
    url = "https://github.com/responsively-org/responsively-app/releases/download/v0.15.0/ResponsivelyApp-0.15.0.AppImage";
    sha256 = "101mg73qalkhbx47xk8swk067x9ll7ff63icgjmmw06xv3d26rgw";
  };
in
wrapType2 {
  inherit name src;

  extraInstallCommands = ''
    mkdir -p $out/share/applications
    echo "[Desktop Entry]
Encoding=UTF-8
Type=Application
Exec=responsively
Name=Responsively App
Version=0.15.0" > $out/share/applications/responsively.desktop
  '';
}
