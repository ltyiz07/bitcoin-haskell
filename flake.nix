{
  description = "A flake for impl-btc Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc912;
        project = haskellPackages.developPackage {
          root = ./.;
          name = "impl-btc";
          modifier =
            drv:
            pkgs.haskell.lib.addBuildTools drv [
              pkgs.pkg-config
              pkgs.zlib
              pkgs.openssl
            ];
        };
      in
      {
        packages.default = project;
        devShells.default = project.env.overrideAttrs (oldAttrs: {
          buildInputs = oldAttrs.buildInputs ++ [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
          ];
        });
      }
    );
}
