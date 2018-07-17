# DESIGN: Can't use the one in nixpkgs because that is Python 2.7 only

nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "geopy";
    version = "1.14.0";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0iwmz997rf4crlvgq3azhw8fvkys2yanzlb4gv2bql8w8cdxdw4x";
    };

    buildInputs = with nixpkgs; [ glibcLocales geographiclib ];

    propagatedBuildInputs = with self; [ geographiclib ];

    preConfigure = ''export LC_ALL="en_US.UTF-8"'';

}
