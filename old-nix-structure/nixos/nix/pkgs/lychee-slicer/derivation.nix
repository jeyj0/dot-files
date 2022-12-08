{ stdenv, appimageTools
, gsettings-desktop-schemas
, gtk3, fetchurl
}:
let
  appImage = fetchurl {
    url = "https://mango-lychee.nyc3.cdn.digitaloceanspaces.com/LycheeSlicer-4.1.0.AppImage";
    sha256 = "sha256-TYGaSOfwThnSE9/x3F1PEWf8j7Ex3r6X5lC4TH9oYIY=";
  };
in
appimageTools.wrapType2 {
  name = "lycheeslicer";

  src = appImage;

  profile = ''
    export LC_ALL=C.UTF-8
    export XDG_DATA_DIRS="${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}:${gtk3}/share/gsettings-schemas/${gtk3.name}:$XDG_DATA_DIRS"
  '';

  extraPkgs = pkgs: (appimageTools.defaultFhsEnvArgs.multiPkgs pkgs) ++ (with pkgs; [
    # fixes "unexpected error"
    gsettings-desktop-schemas glib gtk3

    # needed for icons
    gnome.adwaita-icon-theme
  ]);
}
