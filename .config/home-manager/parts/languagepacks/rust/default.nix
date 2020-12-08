{ pkgs }:
{
  home.packages = with pkgs; [
    rustup
    wasm-pack
  ];
}
