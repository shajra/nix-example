{ buildPythonPackage
, toolz
, example-python-lib
}:

buildPythonPackage {
    name = "example-python-app";
    src = ./.;
    propagatedBuildInputs = [
        toolz
        example-python-lib
    ];
}
