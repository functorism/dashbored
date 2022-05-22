let

  name = "dashbored";
  description = "";
  summary = "";

  ghcVersion = "ghc8107";
  supportedSystems = [ "x86_64-linux" "aarch64-linux" ];

in {

  inherit description;

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2111";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:

    flake-utils.lib.eachSystem supportedSystems (system:

      let

        overlays = [
          haskellNix.overlay
          (final: prev: {
            dashbored = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = ghcVersion;
              shell.tools = {
                cabal = { };
                hlint = { };
                ormolu = { };
                haskell-language-server = { };
              };
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        flake = pkgs.dashbored.flake { };

      in flake // {

        defaultPackage = flake.packages."dashbored:exe:dashbored";

        packages = {
          snap = pkgs.snapTools.makeSnap {
             meta = {
               inherit name description summary;
               architectures = [ "amd64" "aarch64" ];
               confinement = "strict";
               apps.dashbored.command = "${self.defaultPackage."${system}"}/bin/dashbored";
             };
          };
        };

      });

}
