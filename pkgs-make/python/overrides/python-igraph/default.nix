nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "python-igraph";
    version = "0.7.1.post6";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0xp61zz710qlzhmzbfr65d5flvsi8zf2xy78s6rsszh719wl5sm5";
    };

    buildInputs = with nixpkgs; [ igraph pkgconfig ];

    propagatedBuildInputs = with self; [
        pycairo
    ];

}
