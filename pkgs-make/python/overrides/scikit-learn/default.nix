nixpkgs: self: super:

super.scikitlearn.overridePythonAttrs (old: {

    # DESIGN: from Conda build
    patches = [ ./atol_in_gaussian_tests.patch ];

    buildInputs = old.buildInputs ++ [ self.nose-exclude ];

    # DESIGN: https://github.com/scikit-learn/scikit-learn/pull/10723/commits
    checkPhase = ''
        HOME=$TMPDIR OMP_NUM_THREADS=1 \
            nosetests \
            --doctest-options=+SKIP \
            --exclude-test=sklearn.linear_model.tests.test_logistic.test_max_iter \
            $out/${super.python.sitePackages}/sklearn/
    '';

})
