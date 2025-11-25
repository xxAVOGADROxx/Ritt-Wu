{
  description = "Ritt-Wu development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskell.packages.ghc8107.override {
          overrides = self: super: {
            shake = pkgs.haskell.lib.dontCheck (self.callHackage "shake" "0.18.3" {});
          };
        };

        packageName = "Ritt-Wu";
        project = haskellPackages.callCabal2nix packageName ./. {};

      in
      {
        packages.${packageName} = project;
        packages.default = project;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            (haskellPackages.ghcWithHoogle (ps: project.getBuildInputs.haskellBuildInputs))
            haskellPackages.cabal-install
            pkgs.ghcid
            pkgs.hlint
            pkgs.stylish-haskell
            haskellPackages.haskell-language-server
            pkgs.zlib
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.zlib ];
        };
      }
    );
}
