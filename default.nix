let
  pkgs = import <nixpkgs> { };
  compiler = pkgs.haskellPackages;

in compiler.developPackage {
  root = ./.;
  modifier = drv:
  pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
    cabal-install
    ghcid
  ]);
}

