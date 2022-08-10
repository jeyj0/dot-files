{ stdenv, appimageTools
, gsettings-desktop-schemas
, gtk3, fetchurl
}:
let
  appImage = fetchurl {
    url = "https://mango-lychee.nyc3.cdn.digitaloceanspaces.com/LycheeSlicer-3.5.1.AppImage";
    sha256 = "sha256-H5/2mnG6niuM+5KJ40a8Sqhl4pq19dY7ISoR7agGCWs=";
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
    gsettings_desktop_schemas glib gtk3

    # needed for icons
    gnome3.defaultIconTheme
  ]);
}
