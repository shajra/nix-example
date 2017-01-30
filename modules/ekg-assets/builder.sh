source "$stdenv/setup"
mkdir -p "$out"
find "$ekg" -name assets -type d -exec cp -r {} "$out" \;
