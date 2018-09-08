- [About Direnv Nix Integration](#sec-1)
  - [Why Direnv?](#sec-1-1)
  - [The problem solved](#sec-1-2)
- [Installation](#sec-2)
  - [Dependencies](#sec-2-1)
  - [Installing the plugin](#sec-2-2)
- [Usage](#sec-3)
  - [Writing `.envrc` files](#sec-3-1)
  - [Activating the directory](#sec-3-2)


# About Direnv Nix Integration<a id="sec-1"></a>

This provides a replacement/improvement for the Nix support built into [Direnv](https://direnv.net/).

Direnv provides a nice way to manage environment variables by project directory. By placing a file in any directory called `.envrc` with a simple entry, we can then use direnv to associate different environment settings for any file in the directory. Direnv performs this association with scripts, and a standard installation provides a `use_nix` function to pull environment settings from a `nix-shell` invocation of a Nix file.

## Why Direnv?<a id="sec-1-1"></a>

Many tools are already Direnv-aware, including a variety of shell programs, and also text editors. Because Direnv is more well-known and integrated with than Nix, we can use this integration to relieve the burden of having to integrate all these tools with Nix. Also, because Direnv can pull environment variables from much more than Nix, we only have to configure these tools for Direnv.

As an alternative, consider a tool like [nix-buffer](https://github.com/shlevy/nix-buffer) which performs a functionality similar to Direnv, but only for the Emacs editor, and only pulling environment variables with Nix. By separating more concerns, Direnv gives us more flexibility/options.

## The problem solved<a id="sec-1-2"></a>

Unfortunately, there's a few problems with the `use_nix` function that comes standard with Direnv:

1.  If we invoke `nix-collect-garbage`, the files under `/nix/store` that

various environment settings use (like `PATH` or `PYTHONPATH`) can be deleted.

1.  `use_nix` calls `nix-shell` every time we ask Direnv what the environment

settings are for a given file. This might take a few seconds, which can be annoying when programming.

Our replacement provides a new function `use_nix_gcrooted` that addresses both of these problems.

# Installation<a id="sec-2"></a>

## Dependencies<a id="sec-2-1"></a>

As you may have guessed, for this integration to work you must perform the following steps first:

1.  [Install Nix](https://nixos.org/nix/manual/#chap-installation)
2.  [Install Direnv](https://github.com/direnv/direnv#install).

## Installing the plugin<a id="sec-2-2"></a>

If you have a vanilla installation of Direnv, copy or link the provided `direnvrc` file to the configuration directory for Direnv (typically `~/.config/direnv`).

If you already have a `direnvrc` file, edit it to source the one provided.

Now you'll have a new function `use_nix_gcrooted` available for use in `.envrc` files.

# Usage<a id="sec-3"></a>

## Writing `.envrc` files<a id="sec-3-1"></a>

In any directory you can create `.envrc` files that use the `use_nix_gcrooted` function with the following options:

```text
use_nix_gcrooted [-c] [-C CACHE_FILE]... [NIX_FILE]

    NIX_FILE  Nix file to use with nix-shell
	  -c  cache environment settings
	  -C  extra files to watch for cache invalidation
```

If `NIX_FILE` is provided, `nix-shell` is called on it, otherwise `shell.nix` is used if it exists in the same directory as `.envrc`, otherwise `default.nix`. Unlike `use_nix`, `use_nix_gcrooted` doesn't work with nix-shell's `-p` option, and only support Nix files.

`use_nix_gcrooted` creates a `.direnv` directory next to `.envrc`. In this directory, a `.direnv/nix` directory has symlinks that keep generated Nix artifacts from being garbage collected by `nix-collect-garbage`. You can delete this directory if you'd like to free them for collection.

If you use the `-c` option, then environment settings are cached in `.direnv/env.cache`. This cache is invalidated with timestamps. If you make changes and want to recalculate the cache, just touch the Nix file used by the `use_nix_gcrooted` entry in `.envrc` (usually `touch shell.nix`).

Additionally with the `-C` switch, you can specify more files to watch for cache invalidation.

## Activating the directory<a id="sec-3-2"></a>

Once you create the `.envrc` file, you need to activate the directory by changing into it and running

```shell
direnv allow
```

Having scripts dynamically modify environment variables has security implications, so Direnv forces users to manually elect into giving this ability to Direnv per-directory. You have to rerun `direnv allow` if the `.envrc` file changes.
