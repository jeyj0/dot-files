{ lib, fetchzip }:

let
  version = "1.1";
in fetchzip {
  name = "solbera-dnd-fonts";

  url = "https://github.com/jonathonf/solbera-dnd-fonts/archive/v${version}.zip";

  postFetch = ''
    mkdir -p $out/share/fonts
    unzip -j $downloadedFile \*.otf -d $out/share/fonts/
  '';

  sha256 = "0f5ypd7yh5gjzcmmcg8bn52w247z18kwrz0f05hf8kzlzdcab951";

  meta = with lib; {
    description = "A collection of open-source dnd font immitations.";
    homepage = "https://github.com/jonathonf/solbera-dnd-fonts";
  };
}
