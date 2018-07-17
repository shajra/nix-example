nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "pathlib";
    version = "1.0.1";

    src = super.fetchPypi {
      inherit pname version;
      sha256 = "17zajiw4mjbkkv6ahp3xf025qglkj0805m9s41c45zryzj6p2h39";
    };

}
