{ stdenv, appimageTools
, gsettings-desktop-schemas
, gtk3
}:
appimageTools.wrapType2 {
  name = "lycheeslicer";

  src = ./LycheeSlicer-3.3.1.AppImage;

  profile = ''
    export LC_ALL=C.UTF-8
    export XDG_DATA_DIRS="${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}:${gtk3}/share/gsettings-schemas/${gtk3.name}:$XDG_DATA_DIRS"
  '';

  extraPkgs = pkgs: (appimageTools.defaultFhsEnvArgs.multiPkgs pkgs) ++ (with pkgs; [
    # fixes "unexpected error"
    gsettings_desktop_schemas glib gtk3

    # needed for icons
    gnome3.defaultIconTheme
  ]);
}
