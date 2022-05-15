(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hlint = "3.2.7";
    ormolu = "0.1.4.1";
    haskell-language-server = "latest";
  };
}
