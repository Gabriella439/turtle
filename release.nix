let
  default = import ./default.nix { };

in
  { turtle =
      default.aggregate
        { name = "all";

          constituents = [
            default.turtle
          ];
        };
  }
