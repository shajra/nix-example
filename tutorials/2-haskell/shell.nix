(import ./build.nix).env.haskell.withMoreEnvTools (args: [ args.nixpkgs.hello ])
