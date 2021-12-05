{ compiler ? "ghc8107" }:

let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/391f93a83c3a486475d60eb4a569bb6afbf306ad.tar.gz";
    sha256 = "0s5f7j2akh3g0013880jfbigdaac1z76r9dv46yw6k254ba2r6nq";
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
