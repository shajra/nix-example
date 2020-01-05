# used when calling `nix-shell` without any arguments

(import ./build.nix).env.python.withMoreEnvTools (args: [ args.nixpkgs.hello ])
