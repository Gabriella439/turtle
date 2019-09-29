{ compiler ? "ghc864" }:

let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/312a059bef8b29b4db4e73dc02ff441cab7bb26d.tar.gz";

    sha256 = "1j52yvkhw1inp6ilpqy81xv1bbwgwqjn0v9647whampkqgn6dxhk";
  };

  readDirectory = import ./nix/readDirectory.nix;

  config = {
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides =
              let
                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                  system-fileio =
                    pkgs.haskell.lib.dontCheck haskellPackagesOld.system-fileio;
                };

              in
                pkgs.lib.composeExtensions (readDirectory ./nix) manualOverrides;
          };
        };
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { inherit (pkgs.haskell.packages."${compiler}") turtle;

    shell = (pkgs.haskell.packages."${compiler}".turtle).env;

    inherit (pkgs.releaseTools) aggregate;
  }
