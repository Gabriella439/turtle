{ interval ? 24 * 60 * 60 }:

let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/MercuryTechnologies/nixpkgs/archive/696e0820b03e8ea7ad6a9ba21a00a79c91efc580.tar.gz";
    sha256 = "1k3swii3absl154154lmk6zjw11vzzqx8skaiw1250armgfyv9v8";
  };

  # We need GHC 9.4 or newer for this feature to work
  compiler ="ghc94";

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" =
          super.haskell.packages."${compiler}".override (old: {
            overrides =
              self.lib.fold
                self.lib.composeExtensions
                (old.overrides or (_: _: { }))
                [ (self.haskell.lib.packageSourceOverrides {
                    turtle = ./.;
                  })

                  (hself: hsuper: {
                    turtle-incremental =
                      self.haskell.lib.compose.incremental
                        { inherit interval;

                          makePreviousBuild =
                            truncate: (import (truncate ./.) { }).turtle;
                        }
                        hsuper.turtle;
                  })
                ];
          });
      };
    };
  };

  pkgs = import nixpkgs { config = { }; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskell.packages."${compiler}")
      turtle
      turtle-incremental
    ;
  }
