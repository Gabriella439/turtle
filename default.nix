{ compiler ? "ghc865" }:

let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/ae66c3e40486c0e88a6cefc8d275c248fc6a696c.tar.gz";
    sha256 = "1gw4kdlkmxyil8capnagv41hqmh31hkibidjgy3bxhlljr8xgfkc";
  };

  config = {};

  overlay = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
          overrides =
            let
              packageSources = pkgsNew.haskell.lib.packageSourceOverrides {
                "turtle" = ./.;
              };

              directoryOverrides = pkgsNew.haskell.lib.packagesFromDirectory {
                directory = ./nix;
              };

              manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                optparse-applicative =
                  haskellPackagesNew.optparse-applicative_0_16_1_0;
              };

              default = old.overrides or (_: _: {});

            in
              pkgsNew.lib.fold pkgsNew.lib.composeExtensions default [
                packageSources
                directoryOverrides
                manualOverrides
              ];
        });
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskell.packages."${compiler}") turtle;

    shell = (pkgs.haskell.packages."${compiler}".turtle).env;

    inherit (pkgs.releaseTools) aggregate;
  }
