stdenv:
nixpkgs:
drv:

let

    lib = nixpkgs.lib;

    printRefs =
        ''
        echo "[" > "$out"
        while read path
        do
            echo "\"$path\"" >> "$out"
            read ignored
            read numRefs
            for ((i = 0; i < numRefs; i++)); do read ignored_ref; done
        done < graph
        echo "]" >> "$out"
        '';

    dependentDerivations' = acc: worklist:
        let h = builtins.head worklist;
            t = builtins.tail worklist;
            nextAcc =
                if lib.isDerivation h && ! (builtins.elem h acc)
                then [ h ] ++ acc
                else acc;
            filtered =
                builtins.filter (x:
                    lib.isDerivation x && (! builtins.elem x nextAcc));
            filteredH =
                lib.filterAttrs (n: v:
                    n == "propagatedNativeBuildInputs"
                        || n == "nativeBuildInputs") h;
            nextWork = filtered (lib.flatten (lib.attrValues filteredH));
        in
        if worklist == []
        then acc
        else dependentDerivations' nextAcc (nextWork ++ t);

    dependentDerivations = dependentDerivations' [] [ drv ];

    runtimeDependencies =
        import (stdenv.mkDerivation {
            name = "dependencies";
            builder = builtins.toFile "dependencies-builder" "${printRefs}";
            exportReferencesGraph = ["graph" drv];
        });

    licenseRecord = i: r:
        let
            nix = import nixpkgs.path {
                config.allowUnfree = true;
                config.allowBroken = true;
            };
            match = d: lib.isDerivation d && toString d == r;
            nameGuess = builtins.elemAt (lib.splitString "-" r) 1;
            tryAccess = i: n:
                let try = builtins.tryEval nix.${n};
                in if try ? success then try.value else {};
            guessed =
                lib.imap
                tryAccess
                (builtins.filter
                    (n: lib.hasPrefix nameGuess n)
                    (builtins.attrNames nix));
            possibleDerivations = dependentDerivations ++ guessed;
            lookup = lib.findFirst match {} possibleDerivations;
            meta = lookup.meta or null;
            license = meta.license or null;
            homepage = meta.homepage or null;
        in
        { path = r; } // {
            ${if (isNull license) then null else "license"} = license;
            ${if (isNull homepage) then null else "homepage"} = homepage;
        };


in

lib.imap licenseRecord runtimeDependencies
