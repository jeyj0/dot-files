{ pkgs }:
{
  home.packages = with pkgs; [
    libreoffice
    gimp
    inkscape
    # scribus

    vlc
    ffmpeg

    obs-studio
    obs-v4l2sink
  ];
}
