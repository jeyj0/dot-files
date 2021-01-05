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
    xorg.libX11
    xorg.libXcursor
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXi
    alsaLib
    libpulseaudio
    libGL
  ];

  meta = with stdenv.lib; {
    platforms = [ "x86_64-linux" ];
  };
}
