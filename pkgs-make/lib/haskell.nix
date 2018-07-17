nixpkgs:

nixpkgs.haskell.lib // rec {

    cleanSourceIf = cond: f: pkg:
        nixpkgs.haskell.lib.overrideCabal
            pkg
            (args: {
                src =
                    if cond args.src
                    then
                        nixpkgs.lib.sources.cleanSourceWith {
                            filter = f;
                            src = args.src;
                        }
                    else args.src;
            });

    cleanSource = cleanSourceIf (x: true);

    cleanSourceIfLocal = cleanSourceIf nixpkgs.lib.sources.sourceLocal;

}
