nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "torchfile";
    version = "0.1.0";

    src = super.fetchPypi {
        inherit pname version;
        sha256 = "0vhklj6krl9r0kdynb4kcpwp8y1ihl2zw96byallay3k9c9zwgd5";
    };

    propagatedBuildInputs = with self; [
        numpy
    ];

}
