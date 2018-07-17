nixpkgs: self: super:

super.buildPythonPackage rec {

    pname = "torchtext";
    version = "0.2.3";

    src = nixpkgs.fetchFromGitHub {
        owner = "pytorch";
        repo = "text";
        rev = "v${version}";
        sha256 = "0v657xpiicsibvyyv38vqsjd8yj7vzi1rmgj7qm5bz70s0j50zss";
    };

    propagatedBuildInputs = with self; [
        pytorch
        requests
        tqdm
    ];

    doCheck = false;

}
