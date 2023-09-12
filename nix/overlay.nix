final: prev:

let
  haskellOverrides = hfinal: hprev: {
    mpi-hs = hfinal.callPackage ./mpi-hs.nix { };
  };
in
{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // (builtins.mapAttrs
      (key: val: val.override { overrides = haskellOverrides; })
      prev.haskell.packages
    );
  };
}

