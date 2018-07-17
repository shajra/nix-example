# used when calling `nix-shell` without any arguments

(import ./build.nix).env.python.withEnvTools (pkgs: [ pkgs.hello ])
