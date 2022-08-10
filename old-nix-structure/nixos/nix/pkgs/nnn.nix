{ lib, stdenv, fetchFromGitHub, pkgconfig, ncurses, readline }:

with lib;

stdenv.mkDerivation rec {
  pname = "nnn";
  version = "3.4-master";

  src = fetchFromGitHub {
    owner = "jarun";
    repo = pname;
    rev = "75a4123f5a57a07b1d6bc1d35c770b68dcb9c79e";
    sha256 = "017pc6ib18p3dcj64g6qvki9mk04jc0cz64q6jc322gb9f0j7hh9";
  };

  conf = builtins.readFile ./nnn/nnn.h;
  configFile = builtins.toFile "nnn.h" conf;
  preBuild = '' cp ${configFile} src/nnn.h '';

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ readline ncurses ];

  makeFlags = [ "O_NERD=1" "DESTDIR=${placeholder "out"}" "PREFIX=" ];

  # shell completions
  postInstall = ''
    install -Dm555 misc/auto-completion/bash/nnn-completion.bash $out/share/bash-completion/completions/nnn.bash
    install -Dm555 misc/auto-completion/zsh/_nnn -t $out/share/zsh/site-functions
    install -Dm555 misc/auto-completion/fish/nnn.fish -t $out/share/fish/vendor_completions.d

    mkdir -p $out/share/applications
    cp misc/desktop/nnn.desktop $out/share/applications/
  '';

  meta = {
    description = "Small ncurses-based file browser forked from noice";
    homepage = "https://github.com/jarun/nnn";
    license = licenses.bsd2;
    platforms = platforms.all;
    maintainers = with maintainers; [ jfrankenau filalex77 ];
  };
}
