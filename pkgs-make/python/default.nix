defaults:

{ nixpkgs
, pkgs
, pyVersion ? defaults.pyVersion
, overrides ? defaults.overrides
, extraOverrides ? defaults.extraOverrides
, srcFilter ? defaults.srcFilter
, extraSrcFilter ? defaults.extraSrcFilter
, srcTransform ? defaults.srcTransform
, envTools ? defaults.envTools { inherit nixpkgs pyVersion; }
, envPersists ? defaults.envPersists
}:

let

    lib = import ../lib nixpkgs;

    rawPyPkgs = builtins.getAttr ("python" + pyVersion + "Packages") nixpkgs;

    finalOverrides =
        let all = [
            (self: super: pkgs)
            (extraOverrides nixpkgs)
        ];
        in lib.nix.foldr lib.nix.composeExtensions (overrides nixpkgs) all;

    pythonPackages = rawPyPkgs.override { overrides = finalOverrides; };

    filterSource = lib.nix.sources.filterSource
        (lib.nix.sources.allFilters [
            (extraSrcFilter lib)
            (srcFilter lib)
        ]);

    cleanSource = lib.nix.sources.transformSourceIfLocal
        (lib.nix.composed [ (srcTransform lib) filterSource ]);

    callPython = p:
        let expr = if builtins.typeOf p == "path" then import p else p;
        in
        (pythonPackages.callPackage expr {}).overridePythonAttrs (old: {
            src = cleanSource old.src;
            passthru = old.passthru or {} // {
                _isLocalPythonPackage =
                    lib.nix.sources.sourceLocal old.src;
            };
        });

    envPkgs =
        builtins.filter
            (e: e._isLocalPythonPackage or false)
            (builtins.attrValues pkgs);

    envFilter = pkg:
        pkg != null && ! builtins.elem pkg (builtins.attrValues pkgs);

    envArg = a:
        lib.nix.filter envFilter
            (lib.nix.unique
                (builtins.foldl'
                    (acc: s:
                        if builtins.hasAttr a s
                        then builtins.getAttr a s ++ acc
                        else acc)
                    []
                    envPkgs));

    findSetupPy = pkg:
        let
            unfilteredSrc = lib.nix.sources.unfilteredSource pkg.src;
            parent = dirOf unfilteredSrc;
            base = baseNameOf unfilteredSrc;
            type = (builtins.readDir parent).${base} or null;
            isDir = type == "directory";
            root = if isDir then unfilteredSrc else parent;
            setupPyType = (builtins.readDir root)."setup.py" or null;
            hasSetupPy = setupPyType == "regular";
            found = if hasSetupPy then [ "${toString root}/setup.py" ] else [];
        in
            if pkg ? src then found else [];

    setupPys =
        lib.nix.concatStringsSep "\n"
            (lib.nix.unique (lib.nix.concatMap findSetupPy envPkgs));

    env =
        nixpkgs.stdenv.mkDerivation {
            name = "env-python";
            meta.license = lib.nix.licenses.bsd3;
            buildInputs = [
                pythonPackages.setuptoolsBuildHook
                pythonPackages.pipInstallHook
            ];
            nativeBuildInputs = envTools;
            propagatedBuildInputs = envArg "propagatedBuildInputs";
            nativePropagatedBuildInputs =
                envArg "nativePropagatedBuildInputs";
            passthru.withEnvTools = f: env.overrideAttrs (old: {
                nativeBuildInputs = f { inherit nixpkgs pyVersion; };
            });
            passthru.withMoreEnvTools = f: env.overrideAttrs (old: {
                nativeBuildInputs =
                    old.nativeBuildInputs ++ f { inherit nixpkgs pyVersion; };
            });
            shellHook = import ./shellHook.nix {
                inherit setupPys;
                bootstrappedPip = pythonPackages.bootstrapped-pip;
                envPersists = toString envPersists;
                sitePackages = pythonPackages.python.sitePackages;
            };
        };

in

{ inherit pythonPackages callPython env; }
