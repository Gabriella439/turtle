let
  ghc822 = import ./default.nix { compiler = "ghc822"; };

  default = import ./default.nix { };

in
  { turtle =
      default.aggregate
        { name = "all";

          constituents = [
            ghc822.turtle

            default.turtle
          ];
        };
  }
