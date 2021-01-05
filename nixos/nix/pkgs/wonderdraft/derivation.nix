{ unzip, stdenv, autoPatchelfHook
, xorg, libpulseaudio, alsaLib, libGL
}:
let
  version = "1.1.4.2";
  src = ./Wonderdraft-1.1.4.2-Linux64.zip;
in
stdenv.mkDerivation {
  name = "wonderdraft-${version}";

  inherit src;

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out/opt/Wonderdraft/

    unzip ${src} -d $out/opt/Wonderdraft/
    chmod +x $out/opt/Wonderdraft/Wonderdraft.x86_64

    mkdir -p $out/bin
    ln -s $out/opt/Wonderdraft/Wonderdraft.x86_64 $out/bin/wonderdraft
  '';

  nativeBuildInputs = [
    autoPatchelfHook
    unzip
  ];

  buildInputs = [
    # libX11.so.6
    xorg.libX11
    # libXcursor.so.1
    xorg.libXcursor
    # libXinerama.so.1
    xorg.libXinerama
    # libXrandr.so.2
    xorg.libXrandr
    # libXi.so.6
    xorg.libXi
    # libasound.so.2
    alsaLib
    # libpulse.so.0
    libpulseaudio
    # libGL.so.1
    libGL
  ];

  meta = with stdenv.lib; {
    platforms = [ "x86_64-linux" ];
  };
}
