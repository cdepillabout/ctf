{ pkgs ? (import <nixpkgs> {}).pkgs }:

let 
  haskellPackages = pkgs.haskellPackages_ghc783;
in
  haskellPackages.cabal.mkDerivation (self: {
    pname = "ctf";

    version = "0.1.0.0";

    src = ./.;
    #src = builtins.filterSource (path: type: baseNameOf path != "output") ./.;

    isLibrary = false;

    isExecutable = true;

    enableSplitObjs = false;

    buildDepends = [
      haskellPackages.filepath 
      #haskellPackages.hakyll
      
      pkgs.openssl
      pkgs.pcre
      pkgs.zlib
    ];

    buildTools = [
      #ghcMod
      pkgs.haskellPackages.alex
      pkgs.haskellPackages.cabalInstall
      pkgs.haskellPackages.happy

      pkgs.zlib
    ];

    #meta = {
    #  license = self.stdenv.lib.licenses.gpl2;
    #  platforms = self.ghc.meta.platforms;
    #};
  })
