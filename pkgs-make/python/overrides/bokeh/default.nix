nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "bokeh";
    version = "0.13.0";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1mklv3xx3w2d7jx680wn1zdfk3svwfykdkl27cbvfx3w9mvmkkyh";
    };

    propagatedBuildInputs = with self; [
        jinja2
        numpy
        packaging
        python-dateutil
        pyyaml
        six
        tornado
    ];

    checkPhase = ''
        ${self.python.interpreter} -m unittest discover -s bokeh/tests
    '';

    checkInputs = with self; [ mock pytest pillow selenium ];

}
