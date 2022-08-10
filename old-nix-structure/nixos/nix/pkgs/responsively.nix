{ stdenv, fetchurl, appimageTools }:

let
  inherit (appimageTools) wrapType2;

  name = "responsively";
  src = fetchurl {
    url = "https://github.com/responsively-org/responsively-app/releases/download/v0.17.0/ResponsivelyApp-0.17.0.AppImage";
    sha256 = "sha256-2i3CK7OHRSYEkEpfsjUDg6Dzfnd/k+cqb3IWXOMMNwI=";
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
