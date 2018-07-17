nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "diskcache";
    version = "3.0.6";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1wyb4hks977i2c134dnxdsgq0wgwk1gb3d5yk3zhgjidc6f1gw0m";
    };

    doCheck = false;
}
