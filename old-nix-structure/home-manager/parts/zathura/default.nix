{ pkgs }:
let
  zathura-gruvbox = pkgs.fetchFromGitHub {
    owner = "eastack";
    repo = "zathura-gruvbox";
    rev = "3bc76170c788d107f3d3aca35f9a899829099e23";
    sha256 = "19f53ll0kggvhv9f9jyav3bsv845pknvzr1ycjzsl5a20qn9hmdm";
  };
in
{
  home.packages = with pkgs; [
    zathura
  ];

  home.file = {
    zathuraConfig = {
      source = zathura-gruvbox;
      target = ".config/zathura/zathura-gruvbox";
      onChange = "cp ~/.config/zathura/zathura-gruvbox/zathura-gruvbox-dark ~/.config/zathura/zathurarc";
    };
  };
}
