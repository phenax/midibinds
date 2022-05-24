{ nixpkgs ? import <nixpkgs> { }, haskellPackages ? nixpkgs.haskellPackages, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  systemPackages = with pkgs; [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.hpack
    nodePackages.nodemon
    pkgs.zlib
    alsa-lib
  ];

  commonHsPackages = with haskellPackages; [ ];
in
with haskellPackages; mkDerivation {
  pname = "midi-keyboard-shortcuts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = commonHsPackages;
  executableHaskellDepends = commonHsPackages;
  executableSystemDepends = systemPackages;
  testHaskellDepends = commonHsPackages ++ [ hspec ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
