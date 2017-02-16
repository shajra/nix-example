source "$stdenv/setup"


tempdir="$(mktemp --directory)"

find "$input_ref" -maxdepth 1 -mindepth 1 \
    -exec ln -s {} "$tempdir" \;

{
    cat "$deps"
    ls -1d "$tempdir"/*
} \
    | tr '\n' '\0' \
    | tar \
        --create \
        --file "$out" \
        --verbose \
        --gzip \
        --null \
        --transform="s|^${tempdir#/}/||" \
        --files-from -
