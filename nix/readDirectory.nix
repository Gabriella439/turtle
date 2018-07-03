directory:

haskellPackagesNew: haskellPackagesOld:
  let
    haskellPaths = builtins.attrNames (builtins.readDir directory);

    toKeyVal = file: {
      name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

      value = haskellPackagesNew.callPackage (directory + "/${file}") { };
    };

  in
    builtins.listToAttrs (map toKeyVal haskellPaths)
