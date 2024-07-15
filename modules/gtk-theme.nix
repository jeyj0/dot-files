{ config
, pkgs
, lib
, ...
}:
let
  # source: https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/by-name/to/tokyonight-gtk-theme/package.nix
  tokyonight-storm-derivation = {
    lib,
    stdenvNoCC,
    fetchFromGitHub,
    gnome,
    sassc,
    gnome-themes-extra,
    gtk-engine-murrine,
    colorVariants ? [] # default: install all icons
  }:
  let
    pname = "tokyonight-storm-gtk-theme";
    colorVariantList = [
      "dark"
      "light"
    ];
  in
  lib.checkListOfEnum "${pname}: colorVariants" colorVariantList colorVariants

  stdenvNoCC.mkDerivation {
    inherit pname;
    version = "0-unstable-2024-06-27";

    src = fetchFromGitHub {
      owner = "Fausto-Korpsvart";
      repo = "Tokyonight-GTK-Theme";
      rev = "2f566d89856516bef988df3cc32261f752299886";
      hash = "sha256-oKqLb66N4swHfhjUZJIGryE0D9MkuLdKFQa6j3TFmOg=";
    };

    propagatedUserEnvPkgs = [ gtk-engine-murrine ];

    nativeBuildInputs = [ gnome.gnome-shell sassc ];
    buildInputs = [ gnome-themes-extra ];

    dontBuild = true;

    postPatch = ''
      patchShebangs themes/install.sh
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/themes
      cd themes
      ./install.sh -n Tokyonight -c ${lib.concatStringsSep " " (if colorVariants != [] then colorVariants else colorVariantList)} --tweaks storm -d "$out/share/themes"
      runHook postInstall
    '';

    meta = with lib; {
      description = "GTK theme based on the Tokyo Night colour palette";
      homepage = "https://github.com/Fausto-Korpsvart/Tokyonight-GTK-Theme";
      license = licenses.gpl3Plus;
      platforms = platforms.unix;
    };
  };

  tokyonight-storm = pkgs.callPackage tokyonight-storm-derivation {};
in
{
  # GTK 4
  home.file.gtk4-theme-assets = {
    target = ".config/gtk-4.0/assets";
    source = "${tokyonight-storm}/share/themes/Tokyonight-Dark-Storm/gtk-4.0/assets";
  };
  home.file.gtk4-theme-gtk-dark-css = {
    target = ".config/gtk-4.0/gtk-dark.css";
    source = "${tokyonight-storm}/share/themes/Tokyonight-Dark-Storm/gtk-4.0/gtk-dark.css";
  };
  home.file.gtk4-theme-gtk-css = {
    target = ".config/gtk-4.0/gtk.css";
    source = "${tokyonight-storm}/share/themes/Tokyonight-Dark-Storm/gtk-4.0/gtk.css";
  };

  # GTK 3
  home.file.gtk3-theme = {
    target = ".themes/Tokyonight-Dark-Storm";
    source = "${tokyonight-storm}/share/themes/Tokyonight-Dark-Storm";
  };
}
