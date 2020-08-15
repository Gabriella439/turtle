{ compiler ? "ghc865" }:

let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/d8e0ade97ad89cd7ea4452e41b4abcaf7e04a8b7.tar.gz";

    sha256 = "1rm6z9cch0kld1742inpsch06n97qik30a3njglvq52l8g9xw2jj";
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

              manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                system-fileio =
                  pkgsNew.haskell.lib.dontCheck haskellPackagesOld.system-fileio;

                optparse-applicative =
                  haskellPackagesNew.optparse-applicative_0_16_0_0;
              };

              default = old.overrides or (_: _: {});

            in
              pkgsNew.lib.fold pkgsNew.lib.composeExtensions default [
                packageSources
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
