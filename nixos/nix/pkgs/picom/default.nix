{ stdenv, lib, fetchFromGitHub, pkgconfig, uthash, asciidoc, docbook_xml_dtd_45
, docbook_xsl, libxslt, libxml2, makeWrapper, meson, ninja
, xorgproto, libxcb ,xcbutilrenderutil, xcbutilimage, pixman, libev
, dbus, libconfig, libdrm, libGL, pcre, libX11
, libXinerama, libXext, xwininfo, libxdg_basedir }:

stdenv.mkDerivation rec {
  pname = "picom";
  version = "8-jonaburg";

  src = fetchFromGitHub {
    owner  = "jonaburg";
    repo   = "picom";
    rev    = "a8445684fe18946604848efb73ace9457b29bf80";
    sha256 = "154s67p3lxdv9is3lnc32j48p7v9n18ga1j8ln1dxcnb38c19rj7";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [
    meson ninja
    pkgconfig
    uthash
    asciidoc
    docbook_xml_dtd_45
    docbook_xsl
    makeWrapper
  ];

  buildInputs = [
    dbus libX11 libXext
    xorgproto
    libXinerama libdrm pcre libxml2 libxslt libconfig libGL
    libxcb xcbutilrenderutil xcbutilimage
    pixman libev
    libxdg_basedir
  ];

  NIX_CFLAGS_COMPILE = "-fno-strict-aliasing";

  mesonFlags = [
    "-Dbuild_docs=true"
  ];

  installFlags = [ "PREFIX=$(out)" ];

  postInstall = ''
    wrapProgram $out/bin/picom-trans \
      --prefix PATH : ${lib.makeBinPath [ xwininfo ]}
  '';

  meta = with lib; {
    description = "A fork of picom, which is a fork of XCompMgr, a sample compositing manager for X servers";
    longDescription = ''
      A fork of picom, which is a fork of XCompMgr, which is a sample
      compositing manager for X servers supporting the XFIXES, DAMAGE, RENDER,
      and COMPOSITE extensions. It enables basic eye-candy effects. This fork
      adds additional features, such as additional effects, and a fork at a
      well-defined and proper place.
    '';
    # license = licenses.mit;
    homepage = "https://github.com/jonaburg/picom";
    # maintainers = with maintainers; [ ertes enzime twey ];
    platforms = platforms.linux;
  };
}
