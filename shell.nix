let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2021-12-01";
    url =
      "https://github.com/NixOS/nixpkgs/archive/0d02ab20287ebf6bbd249ca18a53a3d7952b05e3.tar.gz";
    sha256 = "sha256:02nwiyx9s5n8ii6rka9ibd0hk42knsmd4l0yzqf85x04yhddk4kl";
  }) { };

in pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv
    (with pkgs.haskellPackages; [ cabal-install ghcid ]);
}
