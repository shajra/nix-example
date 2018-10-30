nixpkgs:

nixpkgs.haskell.lib // rec {

    transformSourceIf = cond: f: pkg:
        nixpkgs.haskell.lib.overrideCabal
            pkg
            (args: {
                src = if cond args.src then f args.src else args.src;
            });

    transformSourceIfLocal = transformSourceIf nixpkgs.lib.sources.sourceLocal;

    filterSourceIf = cond: filter: pkg:
        transformSourceIf cond (src:
            nixpkgs.lib.sources.cleanSourceWith {
                inherit filter src;
            });

    filterSource = filterSourceIf (x: true);

    filterSourceIfLocal = filterSourceIf nixpkgs.lib.sources.sourceLocal;

}
