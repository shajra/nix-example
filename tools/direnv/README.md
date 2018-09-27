- [About Direnv Nix Integration](#org02c7be3)
  - [Why Direnv?](#orgdaf8aff)
  - [The problem solved](#orgb482fea)
- [Installation](#org1a77c42)
  - [Dependencies](#org234e06c)
  - [Installing the plugin](#org05c14ae)
- [Usage](#org4c19b67)
  - [Writing `.envrc` files](#org8a2e06f)
  - [Activating the directory](#orgd4ad110)



<a id="org02c7be3"></a>

# About Direnv Nix Integration

This provides a replacement/improvement for the Nix support built into [Direnv](https://direnv.net/).

Direnv provides a nice way to manage environment variables by project directory. By placing a file in any directory called `.envrc` with a simple entry, we can then use direnv to associate different environment settings for any file in the directory. Direnv performs this association with scripts, and a standard installation provides a `use_nix` function to pull environment settings from a `nix-shell` invocation of a Nix file.


<a id="orgdaf8aff"></a>

## Why Direnv?

Many tools are already Direnv-aware, including a variety of shell programs, and also text editors. Because Direnv is more well-known and integrated with than Nix, we can use this integration to relieve the burden of having to integrate all these tools with Nix. Also, because Direnv can pull environment variables from much more than Nix, we only have to configure these tools for Direnv.

As an alternative, consider a tool like [nix-buffer](https://github.com/shlevy/nix-buffer) which performs a functionality similar to Direnv, but only for the Emacs editor, and only pulling environment variables with Nix. By separating more concerns, Direnv gives us more flexibility/options.


<a id="orgb482fea"></a>

## The problem solved

Unfortunately, there's a few problems with the `use_nix` function that comes standard with Direnv:

1.  If we invoke `nix-collect-garbage`, the files under `/nix/store` that

various environment settings use (like `PATH` or `PYTHONPATH`) can be deleted.

1.  `use_nix` calls `nix-shell` every time we ask Direnv what the environment

settings are for a given file. This might take a few seconds, which can be annoying when programming.

Our replacement provides a new function `use_nix_gcrooted` that addresses both of these problems.


<a id="org1a77c42"></a>

# Installation


<a id="org234e06c"></a>

## Dependencies

As you may have guessed, for this integration to work you must perform the following steps first:

1.  [Install Nix](https://nixos.org/nix/manual/#chap-installation)
2.  [Install Direnv](https://github.com/direnv/direnv#install).


<a id="org05c14ae"></a>

## Installing the plugin

If you have a vanilla installation of Direnv, copy or link the provided `direnvrc` file to the configuration directory for Direnv (typically `~/.config/direnv`).

If you already have a `direnvrc` file, edit it to source the one provided.

Now you'll have a new function `use_nix_gcrooted` available for use in `.envrc` files.


<a id="org4c19b67"></a>

# Usage


<a id="org8a2e06f"></a>

## Writing `.envrc` files

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


<a id="orgd4ad110"></a>

## Activating the directory

Once you create the `.envrc` file, you need to activate the directory by changing into it and running

```shell
direnv allow
```

Having scripts dynamically modify environment variables has security implications, so Direnv forces users to manually elect into giving this ability to Direnv per-directory. You have to rerun `direnv allow` if the `.envrc` file changes.
