{pkgs ? import ../nixpkgs {} }:

let
  inherit (pkgs.haskell) ghcVersion;

  hsPkgs = pkgs.haskell.packages.${ghcVersion};

  pkgDrv = hsPkgs.callCabal2nix "Ritt-Wu" ../.. {};
  haskellDeps = pkgDrv.getBuildInputs.haskellBuildInputs;
  ghc = hsPkgs.ghcWithHoogle (_: haskellDeps);

in
{
  inherit ghc;
  inherit (hsPkgs) cabal-install hlint ghcid stylish-haskell haskell-language-server;
}
