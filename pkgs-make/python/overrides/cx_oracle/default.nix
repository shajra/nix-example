nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "cx_Oracle";
    version = "6.4";

    src = super.fetchPypi {
      inherit pname version;
      sha256 = "09hhq7rk1kzda2m84s3379a1y6y1pvc14sb1ajspqyb58jiy274p";
    };

    buildInputs = with nixpkgs; [ odpic ];

    preConfigure = ''
        export ODPIC_INC_DIR="${nixpkgs.odpic}/include"
        export ODPIC_LIB_DIR="${nixpkgs.odpic}/lib"
    '';

    doCheck = false;

}
