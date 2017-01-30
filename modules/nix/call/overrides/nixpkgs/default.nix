self: super:

{
    rdkafka = (import ./rdkafka.nix) self super;
}
