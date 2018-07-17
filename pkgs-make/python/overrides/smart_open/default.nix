nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "smart_open";
    version = "1.6.0";

    src = super.fetchPypi {
      inherit pname version;
      sha256 = "1nmvpn5l9awgbw1shkwaira1z7p9igi56gnzxyb6wgd04m0v9hn2";
    };

    propagatedBuildInputs = with self; [
        boto
        boto3
        bz2file
        requests
    ];

    checkInputs = with self; [
        mock
        moto
        pathlib2
        responses
    ];

    # DESIGN: depends on a very old version of moto (0.4.31)
    doCheck = false;

}
