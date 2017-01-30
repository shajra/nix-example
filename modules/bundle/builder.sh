source "$stdenv/setup"


mkdir -p "$out/bin"
cp "$service/bin/example-app" "$out/bin"
echo "prepatched GBN: $service/bin/example-app"
echo "   patched GBN: $out/bin/example-app"
chmod u+w "$out/bin/example-app"
strings-replace "$out/bin/example-app" '/nix/store/.*ekg' "$assets"
