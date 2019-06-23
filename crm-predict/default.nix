{ pkgs ? import <nixpkgs> {}} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      labels-csv = dontCheck (doJailbreak (self.callCabal2nix "labels-csv"
        (pkgs.fetchFromGitHub {
          owner = "chrisdone" ;
          repo = "labels" ;
          rev = "v0.3.3" ;
          sha256 = "1hjvj75axc8ph49gwh58gck6vr9h57g04pgx3x8i9b90m0ig1iri" ;
        } + /labels-csv ) {})) ;
    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;
    }) ;
  }

