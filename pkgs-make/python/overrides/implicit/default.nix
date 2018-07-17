nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "implicit";
    version = "0.3.6";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1gfz1nhdixjy5fabn0anif8srd883aqq7asz45plph32ylyk2ypj";
    };

    buildInputs = with self; [ cython ];

    propagatedBuildInputs = with self; [ scipy h5py tqdm ];

    # DESIGN: tests don't exist in source, but incorrectly specified in setup.py
    doCheck = false;

}
