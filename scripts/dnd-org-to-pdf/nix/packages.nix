{ sources ? import ./sources.nix }:
let
  haskellDeps = import ../dependencies.nix;

  overlay = self: super: {
    niv = (import sources.niv {}).niv;

    ghc = self.haskellPackages.ghcWithPackages haskellDeps;
  };
in
  import sources.nixpkgs {
    overlays = [ overlay ];
    config = {};
  }
