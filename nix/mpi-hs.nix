{ mkDerivation
, pkg-config
, base
, bytestring
, c2hs
, lib
, monad-loops
, mpi
, mpiCheckPhaseHook
, openssh
}:
mkDerivation {
  pname = "mpi-hs";
  version = "0.7.2.0";
  src = lib.cleanSource ./..;
  isLibrary = true;
  isExecutable = true;
  buildTools = [ pkg-config ];
  pkg-configDepends = [ mpi ];
  libraryHaskellDepends = [ base bytestring monad-loops ];
  libraryToolDepends = [ c2hs ];
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ mpi ];
  testHaskellDepends = [ base monad-loops ];
  testSystemDepends = [ openssh mpi mpiCheckPhaseHook] ;
  homepage = "https://github.com/eschnett/mpi-hs#readme";
  description = "MPI bindings for Haskell";
  license = lib.licenses.asl20;
}
