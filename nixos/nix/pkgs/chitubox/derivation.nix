{ lib, stdenv, autoPatchelfHook
, libcxx, libcxxStdenv, qt5
}:
let
  version = "1.8.1";
  src = ./CHITUBOX_V1.8.1.tar.gz;
in
stdenv.mkDerivation {
  pname = "chitubox";
  version = version;

  inherit src;

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out

    # unzip ${src} -d $out/opt/Wonderdraft/

    tar xf ${src} -C $out/
  '';

  nativeBuildInputs = [
    autoPatchelfHook
    # unzip
  ];

  buildInputs = [
    # libstdcxx5

    qt5.qtbase.out
    qt5.qtdeclarative.out
    stdenv.cc.cc.lib

    # xorg.libX11
    # xorg.libXcursor
    # xorg.libXinerama
    # xorg.libXrandr
    # xorg.libXi
    # alsaLib
    # libpulseaudio
    # libGL
  ];

  meta = with lib; {
    platforms = [ "x86_64-linux" ];
  };
}
