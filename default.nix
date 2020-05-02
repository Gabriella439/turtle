{ compiler ? "ghc864" }:

let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/312a059bef8b29b4db4e73dc02ff441cab7bb26d.tar.gz";

    sha256 = "1j52yvkhw1inp6ilpqy81xv1bbwgwqjn0v9647whampkqgn6dxhk";
  };

  config = {};

  overlay = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
          overrides =
            let
              packageSources = pkgsNew.haskell.lib.packageSourceOverrides {
                "fail" = "4.9.0.0";

                "turtle" = ./.;
              };

              manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                system-fileio =
                  pkgsNew.haskell.lib.dontCheck haskellPackagesOld.system-fileio;
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
