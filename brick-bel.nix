{ pkgs ? import <nixpkgs> {} }: 

builtins.fetchTarball
  "https://github.com/functorism/brick-bel/archive/fcc6c49e.tar.gz"
