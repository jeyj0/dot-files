{ stdenv, fetchurl, appimageTools }:

let
  inherit (appimageTools) wrapType2;

  name = "responsively";
  src = fetchurl {
    url = "https://github.com/manojVivek/responsively-app/releases/download/v0.4.0/ResponsivelyApp-0.4.0.AppImage";
    sha256 = "0n17hipkys3zhihpspll2bx40338fikq428i50bp5zip9gd1i1r7";
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
Version=0.4.0" > $out/share/applications/responsively.desktop
  '';
}
