{ lib
, stdenv
, pkgs
, ...
}:
let
  tree-sitter-typst-github = {
    owner = "frozolotl";
    repo = "tree-sitter-typst";
    rev = "bb7682d2722517ef494c929af82e80d697e7fd69";
    sha256 = "sha256-tiXwYFfz36ql4PS3RHV3dYmsPeUxGP0gUziPSD3Q9BI=";
  };
in
stdenv.mkDerivation rec {
  # taken and modified from https://github.com/helix-editor/helix/blob/3a8592abdb9f30c7ebb74f10fa5127e754d18bc6/grammars.nix#L51
  pname = "helix-tree-sitter-typst";
  version = tree-sitter-typst-github.rev;

  src = pkgs.fetchFromGitHub tree-sitter-typst-github;

  dontUnpack = true;
  dontConfigure = true;

  FLAGS = [
    "-I${src}/src"
    "-g"
    "-O3"
    "-fPIC"
    "-fno-exceptions"
    "-Wl,-z,relro,-z,now"
  ];

  buildPhase = ''
    runHook preBuild

    if [[ -e "$src/src/scanner.cc" ]]; then
      $CXX -c "$src/src/scanner.cc" -o scanner.o $FLAGS
    elif [[ -e "$src/src/scanner.c" ]]; then
      $CC -c "$src/src/scanner.c" -o scanner.o $FLAGS
    fi

    $CC -c "$src/src/parser.c" -o parser.o $FLAGS
    $CXX -shared -o typst.so *.o

    ls -al

    runHook postBuild
  '';

  # NOTE added copying of queries to final result, as to allow using it in other modules
  #      (specifically the helix module)
  installPhase = ''
    runHook preInstall
    mkdir $out
    mv typst.so $out/
    cp -r ${src + "/queries"} $out/
    runHook postInstall
  '';

  fixupPhase = ''
    runHook preFixup
    $STRIP $out/typst.so
    runHook postFixup
  '';
}