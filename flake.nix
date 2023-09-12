{
  description = "MPI bindings for Haskell";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    allow-import-from-derivation = "true";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachSystem [ "x86_64-linux" ]
    (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          #config.allowUnsupportedSystem = true;
          overlays = [ (import ./nix/overlay.nix) ];
        };
      in
      {
        packages = {
          default = self.packages."${system}".mpi-hs_lmpi;
          mpi-hs_openmpi = pkgs.haskell.lib.appendConfigureFlags (pkgs.haskellPackages.mpi-hs.override { mpi = pkgs.mpi; }) [ "-fopenmpi" "-f-mpich" "-f-mvapich" ];
          mpi-hs_mpich = pkgs.haskell.lib.appendConfigureFlags (pkgs.haskellPackages.mpi-hs.override { mpi = pkgs.mpich; }) [ "-fmpich" "-f-openmpi" "-f-mvapich" ];
          mpi-hs_mvapich = pkgs.haskell.lib.appendConfigureFlags (pkgs.haskellPackages.mpi-hs.override { mpi = pkgs.mvapich; }) [ "-fmvapich" "-f-openmpi" "-f-mpich" ];
          mpi-hs_lmpi = pkgs.haskell.lib.appendConfigureFlags (pkgs.haskellPackages.mpi-hs.override { mpi = pkgs.mpi; }) [ "-f-mvapich" "-f-openmpi" "-f-mpich" ];
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          withHoogle = true;
          packages = p: [ p.mpi-hs ];
          buildInputs = with pkgs; [
            cabal-install
            cabal2nix
            haskell-language-server
            haskellPackages.hls-fourmolu-plugin
            haskellPackages.fourmolu
            hlint
            hpack
            nixpkgs-fmt
            pkg-config
            mpi
            mvapich
            mpich
          ];
        };

        formatter = pkgs.nixpkgs-fmt;
      }) // {
    overlays.default = import ./nix/overlay.nix;

    checks.x86_64-linux = self.packages.x86_64-linux;
  };
}
