source "$stdenv/setup"


BIN="nix-tags-haskell"


mkdir -p "$out/bin"
cp "$src/$BIN" "$out/bin/$BIN"
chmod u+x "$out/bin/$BIN"
wrapProgram "$out/bin/$BIN" \
    --prefix PATH : "${hasktags}/bin" \
    --prefix PATH : "${haskdogs}/bin" \
    --prefix PATH : "${which}/bin"
