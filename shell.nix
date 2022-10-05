{ pkgs ? import <nixpkgs> {} }: 

pkgs.haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
    brick-bel = import ./brick-bel.nix { inherit pkgs; };
  };

  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install
        ghcid
        haskell-language-server
        ormolu
      ]);
}
