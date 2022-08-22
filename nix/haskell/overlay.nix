nixpkgsSelf: nixpkgsSuper:

let
  inherit (nixpkgsSelf) pkgs;

  ghcVersion = "ghc8107";

  hsPkgs = nixpkgsSuper.haskell.packages.${ghcVersion}.override {
    overrides = self: super: {
      shake = pkgs.haskell.lib.dontCheck (self.callHackage "shake" "0.18.3" {});
    };
  };

in
{
  haskell = nixpkgsSuper.haskell // {
    inherit ghcVersion;

    packages = nixpkgsSuper.haskell.packages // {
      "${ghcVersion}" = hsPkgs;
    };
  };
}
