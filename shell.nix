let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2020-07-31";
    url =
      "https://github.com/NixOS/nixpkgs/archive/7a1dfb5b3d0a6c750e17ad9ff0a2c182e63d0ef6.tar.gz";
    sha256 = "sha256:1dvj48zw175n739qvapgcfj3abb3klzppx5md9ypd4ahqkcr3ww0";
  }) { };

in pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv
    (with pkgs.haskellPackages; [ cabal-install ghcid ]);
}
