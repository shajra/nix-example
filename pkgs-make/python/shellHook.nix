{ bootstrappedPip
, envPersists
, setupPys
, sitePackages
}:

''
tmp_path="$(mktemp -d)"
trap "rm -rf \"$tmp_path\"" EXIT

mkdir -p "$tmp_path/${sitePackages}"

pip_install_hash="$({
        echo "${setupPys}" | { while read -r f; do sha1sum "$f"; done; }
        find "${bootstrappedPip}/bin" -type f -exec sha1sum {} +
    } | sort | sha1sum | cut -d ' ' -f 1)"

persisted_dir="$(dirname "$tmp_path")/tmp.$pip_install_hash"

if [ "${envPersists}" = "" ] || ! [ -e "$persisted_dir" ]
then
    echo "${setupPys}" | {
        while read -r f
        do
            setup_dir="$(dirname "$f")"
            pushd "$setup_dir"
            PYTHONPATH="$tmp_path/${sitePackages}:$PYTHONPATH" \
                ${bootstrappedPip}/bin/pip install \
                -e . \
                --prefix "$tmp_path" >&2
            popd
        done
    }
fi

if [ "${envPersists}" = "1" ]
then
    if ! [ -e "$persisted_dir" ]
    then cp -r "$tmp_path" "$persisted_dir"
    fi
    export PATH="$persisted_dir/bin:$PATH"
    export PYTHONPATH="$persisted_dir/${sitePackages}:$PYTHONPATH"
    echo "nix-shell: state persisted: $persisted_dir" >&2
else
    export PATH="$tmp_path/bin:$PATH"
    export PYTHONPATH="$tmp_path/${sitePackages}:$PYTHONPATH"
fi

unset tmp_path
unset pip_install_hash
unset persisted_dir
''
