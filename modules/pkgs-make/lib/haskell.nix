lib:

lib // {

    filterSource = f: drv:
        lib.overrideCabal
            drv
            (args: { src = builtins.filterSource f args.src; });

}
