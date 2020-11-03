{ stdenv, fetchurl, appimageTools }:

let
  inherit (appimageTools) wrapType2;

  name = "dat-desktop";
  src = fetchurl {
    url = "https://github.com/datproject/dat-desktop/releases/download/v3.0.1/Dat-Desktop-3.0.1.AppImage";
    sha256 = "1rqcy7xy4wm8fn98pv3vqkxl6c5602alxmh9vgyh39lw5nx5i3h4";
  };
in
wrapType2 {
  inherit name src;

  extraInstallCommands = ''
    mkdir -p $out/share/applications
    echo "[Desktop Entry]
Encoding=UTF-8
Type=Application
Exec=dat-desktop
Name=Dat Desktop
Version=3.0.1" > $out/share/applications/dat-desktop.desktop
  '';
}
