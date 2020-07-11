# Haskell Katas

![Test](https://github.com/alskipp/haskell-katas/workflows/Test/badge.svg)

Some random programming problems in Haskell. All code is in the `test` directory.

This project will hopefully serve as a useful reference to anyone interested in trying out programming problems using Haskell with continuous test feedback.

[Nix](https://nixos.org/nix/) is recommended to get up and running.

## TL;DR

Assuming Nix is installed, run `bin/test`, all tests will run and will rerun each time a file changes.

## Developing Katas and Running tests

To start a new kata, add a new file to the `test` directory (the file name must end with `Spec.hs`), also add the file name to the end of `haskell-katas.cabal`.

[Hspec](https://hspec.github.io) is used for testing, so a `spec` function will be needed.
<details>
<summary>Here is a scaffold for a new file:</summary>

``` haskell
module WibbleSpec where

import Test.Hspec

spec :: Spec
spec = describe "Testy time" $ do
  it "should work" $ do
    pending
```
</details>

To have unit tests run automatically upon file save with [ghcid](https://github.com/ndmitchell/ghcid), run the following in a terminal:

``` sh
bin/test
```

On first run, the ghc compiler and project dependencies will be installed into the nix store (subsequent runs will use the binaries in your local nix store).

## Installing Nix

Instructions for [Linux and pre macOS Catalina](https://nixos.org/download.html)

For macOS Catalina users the recommended method is [here](https://nixos.org/nix/manual/#sect-macos-installation)

## Other Haskell projects using Nix

[Neuron](https://github.com/srid/neuron)
