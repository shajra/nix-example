pkgs:

rec {

    inherit (pkgs.haskell.lib)
        sdistTarball
        buildFromSdist
        buildStrictly
        buildStackProject;

    filterSource = f:
        overrideCabal (args: { src = builtins.filterSource f args.src; });

    doHaddock = overrideCabal (args: { doHaddock = true; });
    dontHaddock = overrideCabal (args: { doHaddock = false; });

    doJailbreak = overrideCabal (args: { jailbreak = true; });
    dontJailbreak = overrideCabal (args: { jailbreak = false; });

    doCheck = overrideCabal (args: { doCheck = true; });
    dontCheck = overrideCabal (args: { doCheck = false; });

    doDistribute = overrideCabal (args: {
        hydraPlatforms =
            args.platforms
                or ["i686-linux" "x86_64-linux" "x86_64-darwin"];
    });
    dontDistribute = overrideCabal (args: { hydraPlatforms = []; });

    appendConfigureFlag = x: overrideCabal (args: {
        configureFlags = (args.configureFlags or []) ++ [x];
    });
    removeConfigureFlag = x: overrideCabal (args: {
        configureFlags =
            pkgs.stdenv.lib.remove x (args.configureFlags or []);
    });

    addBuildTool = x: addBuildTools [x];
    addBuildTools = xs:
        overrideCabal (args: { buildTools = (args.buildTools or []) ++ xs; });

    addExtraLibrary = x: addExtraLibraries [x];
    addExtraLibraries = xs: overrideCabal (args: {
        extraLibraries = (args.extraLibraries or []) ++ xs;
    });

    addBuildDepend = x: addBuildDepends [x];
    addBuildDepends = xs: overrideCabal (args: {
        buildDepends = (args.buildDepends or []) ++ xs;
    });

    addPkgconfigDepend = x: addPkgconfigDepends [x];
    addPkgconfigDepends = xs: overrideCabal (args: {
        pkgconfigDepends = (args.pkgconfigDepends or []) ++ xs;
    });

    enableCabalFlag = x: args:
        appendConfigureFlag "-f${x}" (removeConfigureFlag "-f-${x}" args);
    disableCabalFlag = x: args:
        appendConfigureFlag "-f-${x}" (removeConfigureFlag "-f${x}" args);

    markBroken = overrideCabal (args: { broken = true; });
    markBrokenVersion = version: args:
        assert args.version == version;
        markBroken args;

    enableLibraryProfiling =
        overrideCabal (args: { enableLibraryProfiling = true; });
    disableLibraryProfiling =
        overrideCabal (args: { enableLibraryProfiling = false; });

    enableSharedExecutables =
        overrideCabal (args: { enableSharedExecutables = true; });
    disableSharedExecutables =
        overrideCabal (args: { enableSharedExecutables = false; });

    enableSharedLibraries =
        overrideCabal (args: { enableSharedLibraries = true; });
    disableSharedLibraries =
        overrideCabal (args: { enableSharedLibraries = false; });

    enableDeadCodeElimination =
        overrideCabal (args: { enableDeadCodeElimination = true; });
    disableDeadCodeElimination =
        overrideCabal (args: { enableDeadCodeElimination = false; });

    enableStaticLibraries =
        overrideCabal (args: { enableStaticLibraries = true; });
    disableStaticLibraries =
        overrideCabal (args: { enableStaticLibraries = false; });

    appendPatch = x: appendPatches [x];
    appendPatches = xs:
        overrideCabal (args: { patches = (args.patches or []) ++ xs; });

    doHyperlinkSource = overrideCabal (args: { hyperlinkSource = true; });
    dontHyperlinkSource = overrideCabal (args: { hyperlinkSource = false; });

    disableHardening = flags:
        overrideCabal (args: { hardeningDisable = flags; });

    triggerRebuild = i:
        overrideCabal (args: {
            postUnpack = ": trigger rebuild ${toString i}";
        });

    overrideCabal = f: drv: drv.override (args: args // {
        mkDerivation = attrs: (args.mkDerivation attrs).override f;
    });

}
