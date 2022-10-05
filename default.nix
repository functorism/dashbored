{ pkgs ? import <nixpkgs> {} }: 

(pkgs.haskellPackages.override (old: {
  overrides = pkgs.haskell.lib.packageSourceOverrides {
    brick-bel = import ./brick-bel.nix { inherit pkgs; };
  };
})).callCabal2nix "dashbored" (./.) {}
