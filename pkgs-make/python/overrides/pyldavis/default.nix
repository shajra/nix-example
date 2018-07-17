nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "pyLDAvis";
    version = "2.1.2";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "08psl2ad9zgi1nnriglh5yl96wp8yfb9icab6pp8xa4gk8i42802";
    };

    # DESIGN: tests failing because of column-order differences
    # This is likely just because of moving to a new version of
    # Pandas
    #
    prePatch = ''
        substituteInPlace tests/pyLDAvis/test_prepare.py \
            --replace \
            "assert_frame_equal(eddf, oddf)" \
            "assert_frame_equal(eddf, oddf, check_like=True)"

        substituteInPlace tests/pyLDAvis/test_prepare.py \
            --replace \
            "check_less_precise=True)" \
            "check_less_precise=True, check_like=True)"
    '';

    propagatedBuildInputs = with self; [
        funcy
        future
        jinja2
        joblib
        numexpr
        numpy
        pandas
        scipy
    ];

    checkInputs = with self; [ pytest gensim ];

}
