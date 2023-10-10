{
  description = "NixOS installer";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hp = pkgs.haskellPackages;
        haskellLib = pkgs.haskell.lib;
        packageName = "nixos-installer";
        doJailbreak = pkg: haskellLib.dontCheck (haskellLib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; })));

      in
      {
        packages.${packageName} = hp.callCabal2nix packageName self rec {
          h-gpgme = doJailbreak hp.h-gpgme;
        };
        defaultPackage = self.packages.${system}.${packageName};


        devShell = with pkgs; mkShell {
          buildInputs = [
            # Haskell
            hp.ghc
            hp.cabal-install
            hp.cabal-fmt
            hp.fourmolu
            hp.haskell-language-server
            hp.hlint
            # GPGME library
            gpgme
            # Misc
            treefmt
            nixpkgs-fmt
          ];
        };
      });

}
