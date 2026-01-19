final: prev:

let
  addMpiHsTestDeps = drv:
    final.haskell.lib.compose.addTestToolDepends
      [ final.openssh final.mpiCheckPhaseHook ]
      drv;
in
{
  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev:
      prev.haskell.packageOverrides hfinal hprev // {
        mpi-hs = hfinal.mpi-hs_openmpi;

        mpi-hs_openmpi =
          addMpiHsTestDeps
            (hfinal.callCabal2nixWithOptions "mpi-hs" ../. "-fopenmpi -f-mpich -f-mvapich" {
              ompi = final.mpi;
            });

        mpi-hs_mpich =
          addMpiHsTestDeps
            (hfinal.callCabal2nixWithOptions "mpi-hs" ../. "-f-openmpi -fmpich -f-mvapich" {
              mpich = final.mpich;
            });

        mpi-hs_mvapich =
          addMpiHsTestDeps
            (hfinal.callCabal2nixWithOptions "mpi-hs" ../. "-f-openmpi -f-mpich -fmvapich" {
              mvapich = final.mvapich;
            });

        mpi-hs_mpi =
          addMpiHsTestDeps
            (hfinal.callCabal2nixWithOptions "mpi-hs" ../. "-f-openmpi -f-mpich -f-mvapich" {
              openmpi = final.mpi;
            });
      };
  };
}

