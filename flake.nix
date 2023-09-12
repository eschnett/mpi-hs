{
  description = "MPI bindings for Haskell";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
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
          default = self.packages."${system}".mpi-hs_openmpi;
          mpi-hs_openmpi = pkgs.haskellPackages.mpi-hs_openmpi;
          mpi-hs_mpich = pkgs.haskellPackages.mpi-hs_mpich;
          mpi-hs_mvapich = pkgs.haskellPackages.mpi-hs_mvapich;
          mpi-hs_mpi = pkgs.haskellPackages.mpi-hs_mpi;
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
