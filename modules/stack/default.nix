{ pkgs
, blas
, gfortran
, git
, gmp
, liblapack
, rdkafka
, zlib
}:
{ghc}:
pkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "myEnv";
    buildInputs = [
        blas
        gfortran.cc
        git
        gmp
        (liblapack.override { shared = true; })
        rdkafka
        zlib
    ];
}
