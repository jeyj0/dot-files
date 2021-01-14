{ pkgs }:
let
  fetchFromGitHub = pkgs.fetchFromGitHub;
in
{
  home.packages = with pkgs; [
    texlive.combined.scheme-full
  ];

  home.file."texmf/tex/latex/dnd".source = fetchFromGitHub {
    owner = "rpgtex";
    repo = "DND-5e-LaTeX-Template";
    rev = "c2a4eb1e21073096a526f0fd122727b47a8ae398";
    sha256 = "072py35sl4fil5a0zfhcj3ih5abiyjqqwdc2d4a6la3f4z9049ld";
  };
}
