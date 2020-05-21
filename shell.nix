let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2020-05-21";
    url =
      "https://github.com/NixOS/nixpkgs/archive/9dfcff3a1c98bf4b0b3366fe1e02893e01d48da5.tar.gz";
    sha256 = "1ayc8pv04f9lgiflqwmkayw53cranb2kb1isiqmlsaydsqvvyhm0";
  }) { };

in pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv
    (with pkgs.haskellPackages; [ cabal-install ghcid ]);
}
