{ stdenv }:
nixpkgs:

let

    lib = nixpkgs.lib;

    recursive = f: d:
        if lib.isDerivation d
        then f d
        else
            lib.mapAttrsRecursiveCond
                (x: ! lib.isDerivation x)
                (n: v:  if lib.isDerivation v then f v else null)
                d;

    report = import ./report.nix stdenv nixpkgs;

in

rec {

    report = recursive (import ./report.nix stdenv nixpkgs);

    json = d:
        stdenv.mkDerivation (rec {
            name = (if d ? name then d.name + "." else "") + "license.json";
            input = builtins.toFile name (builtins.toJSON (report d));
            builder = builtins.toFile "builder.sh"
                ''
                source $stdenv/setup
                ln -s $input $out
                '';
        });

}
