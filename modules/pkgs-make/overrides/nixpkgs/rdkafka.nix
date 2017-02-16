self: super:

super.stdenv.mkDerivation {
    name = "rdkafka";
    version = "0.9.3";
    src = super.fetchFromGitHub {
        owner = "edenhill";
        repo = "librdkafka";
        # $ git describe v0.9.3 --tags --long
        # v0.9.3-0-g2f153ea
       rev = "2f153ea92a521bf7d2eb6b3108f393caafab3809";
       sha256 = "01crhzqmrxp38gnzz6ydj81mm10vy9vxvr5qharpj2ww0blqbvi1";
    };
    buildInputs = [ super.zlib super.python ];
    NIX_CFLAGS_COMPILE = "-Wno-error=strict-overflow";
    postPatch = ''patchShebangs .'';
}
