source "$stdenv/setup"


mkdir -p "$out/bin"
cp "$service/bin/example-app" "$out/bin"
echo "prepatched GBN: $service/bin/example-app"
echo "   patched GBN: $out/bin/example-app"
chmod u+w "$out/bin/example-app"
replace-literal -b -e -z -nn "$ekg" "$assets" "$out/bin/example-app"
replace-literal -b -e -z -f "$ekg" "$assets" "$out/bin/example-app"
