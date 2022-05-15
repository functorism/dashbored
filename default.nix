let

  sources = import ./nix/sources.nix {};

  haskellNix = import sources.haskellNix {};

  pkgs = import
    haskellNix.sources.nixpkgs-unstable
    haskellNix.nixpkgsArgs;

in pkgs.haskell-nix.project {

  compiler-nix-name = "ghc8107";

  index-state = "2022-04-29T00:00:00Z";

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "dashbored";
    src = ./.;
  };

}
