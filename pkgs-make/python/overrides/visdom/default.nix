nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "visdom";
    version = "0.1.8.4";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "1rchl50xs60zhsqs3ip028619zxly50srjk4vgawgbipplkyxbbw";
    };

    propagatedBuildInputs = with self; [
        numpy
        pillow
        pyzmq
        requests
        scipy
        six
        torchfile
        tornado
        websocket_client
    ];

    doCheck = false;  # broken, but also no tests to run

}
