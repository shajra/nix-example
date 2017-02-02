# Example Nix Project

This project illustrates using Nix for programming multi-module projects.
Some aspects of it are specialized for Haskell development.  It's possible it
could be extended for other language platforms.

Use this project to learn and explore what's possible.  Don't expect it to be
a general solution for every possible scenario.  I'm okay supporting the
project as pedagogy, but not as a dependency for real teams/projects.  There's
just too many ways to factor code, and the beauty of Nix is that it's very
flexible.  This is just one way to use it; feel free to fork and modify it.

Gabriel Gonzalez has a similar project
[haskell-nix](https://github.com/Gabriel439/haskell-nix).  Gabriel's project has
more tutorial content.  Hopefully projects like these will cross-pollinate and
reinforce one another.  Not wanting to make yet-another tutorial, I tried to
make the code in this repository clean and self-documenting.  If you have a way
to improve its clarity, please submit an issue or pull request.


# Features

Here's some language-agnostic features of this project:

- supports mixed platform build (C, Haskell, almost anything else) via Nix
- managed through a "call-package"-style Nix DSL in `build.nix`
    - concise
    - allows overriding of `nixpkgs`
    - comes with some baked-in overrides (just for illustration, you can
      change them)
    - pins [NixOS/nixpkgs](https://github.com/NixOS/nixpkgs) to a specified
      version for deterministic builds (`<nixpkgs>`/`NIX_PATH` only used for
      bootstrapping)
- factored into modules (Nix expressions get twisted easily)
- supports simple invocations of `nix-build`, `nix-env`, and `nix-shell`
- supports ignoring files
    - avoids needless rebuilds
    - keeps ignored files out of /nix/store

Here's some features specific to Haskell:

- automatically calls `cabal2nix` for you (Haskell projects are Nix-free)
- supports compact Haskell statically linked binaries
- has workaround to keep compact even if dependencies use Cabal "data-files"
  (`replace-literal`)
- supports simple invocations of `stack`
- supports `cabal` "new-*" builds (using `nix-shell`)
- supports `ghcid` (using `nix-shell`)


# Prerequisites

Because this project illustrates how to use Nix, you should
have [Nix installed](https://nixos.org/).

Also, though theoretically this project may work on Macs, it's only been tested
within a Linux OS.  Nix support for Macs is always improving, but is known to
be idiosyncratic.

Optionally, if you have [Haskell Stack](http://haskellstack.org) installed,
this project illustrates one way to integrate with Stack.


# Quick Start

Without over-explaining everything in this project, let's first just see it in
action.  If you have this repository cloned, you should be able to build it
with a simple call to `nix-build` from the project's root:

```
example-nix$ nix-build
fetching path ‘/nix/store/i4aas1bbzaryxsn9rpld6971w41p5076-mirrors-list’...
fetching path ‘/nix/store/jgx4m6z00r445qpsldh05chmq1lniar1-curl-7.52.1-man’...
fetching path ‘/nix/store/m08mp1p349yq21ziii7a4il7knsahbzk-diffutils-3.5’...
fetching path ‘/nix/store/dfcl6br2sic5cqnvfxdns4xxsqdczq29-ed-1.14.1’...
...
building path(s) ‘/nix/store/8p8k06039cya3rylh8swx2jwk5146psh-example-bundle’
prepatched GBN: /nix/store/qa550kcqizy5vx142rf4820yn1lzay40-example-app-0.1.0.0/bin/example-app
   patched GBN: /nix/store/8p8k06039cya3rylh8swx2jwk5146psh-example-bundle/bin/example-app
0+1 records in
55+0 records out
55 bytes copied, 7.4953e-05 s, 734 kB/s
/nix/store/8p8k06039cya3rylh8swx2jwk5146psh-example-bundle
```

The `nix-build` invocation will produce a "result" symlink in the current
directory:

```
result
└── bin
    └── example-app
```

You can run this application:

```
example-nix$ result/bin/example-app
EKG running on http://localhost:8081
hit any key to quit
```

The example application runs `ekg`, a Haskell monitoring service, which you
can see running using the URL reported.  When you hit a key, it stops.

The application is statically linked, and the transitively closed set of
references back into nix-store are minimal:

```
example-nix$ nix-store --query --requisites result | xargs du -sh
23M	/nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24
4.7M	/nix/store/jar52969wyf10sh2wj62ipfjiw7xaq2j-gcc-5.4.0-lib
684K	/nix/store/a9wlwd8cfzyprwwkib5ibi3cn56v165y-gmp-6.1.1
220K	/nix/store/crn26c4canjpjbm6cxq2kb1z4i1q8y60-ekg-assets
124K	/nix/store/gdw5qz99ihbv3l3nc91jdynfm3va46qq-zlib-1.2.10
3.4M	/nix/store/nxcny3iwkq6bn0y16l2m6jbrblwnwmrl-openssl-1.0.2j
2.7M	/nix/store/8p8k06039cya3rylh8swx2jwk5146psh-example-bundle
```

If you like, you can install the application into your Nix profile, so it's on
your shell's path (rather than having to call it through the "result" symlink).

```
example-nix$ nix-env --install --file .
installing ‘example-bundle’

example-nix$ which example-app
~/.nix-profile/bin/example-app
```

This project also creates a environment for `nix-shell` that comes with:

- `ghc`, preloaded with all your dependencies
- `cabal-install`
- `ghcid`

The version of `cabal-install` is recent enough that it includes
the
["new-build" features](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/) supporting
multiple projects:

```
example$ nix-shell --command '
    cabal update;
    cabal new-configure;
    cabal new-build'
...
Downloading the latest package list from hackage.haskell.org
Resolving dependencies...
In order, the following would be built (use -v for more details):
example-lib-0.1.0.0
example-app-0.1.0.0
Configuring example-lib-0.1.0.0...
Preprocessing library example-lib-0.1.0.0...
...
Linking dist-newstyle/build/example-app-0.1.0.0/build/example-app/example-app ...
```

The last line shows where your cabal-built binary can be found.  It's linked
and ready to run.

From `nix-shell` you can run `ghcid`, which some people like for fast
incremental compilation while developing.

```
example-nix: nix-shell
[nix-shell:example-nix]$ cd modules/app
[nix-shell:app]$ cd modules/app
[nix-shell:app]$ ghcid --command 'cabal new-repl'
```

Finally, if you have `stack` installed, you can run it from the root project:

```
example-nix$ stack build
...
example-lib-0.1.0.0: configure (lib)
example-lib-0.1.0.0: build (lib)
example-lib-0.1.0.0: copy/register
example-app-0.1.0.0: configure (exe)
example-app-0.1.0.0: build (exe)
example-app-0.1.0.0: copy/register
Completed 2 action(s).
Log files have been written to: example-nix/.stack-work/logs/
```

System dependencies for `stack` come from the Nix configuration, but be aware
that `stack` manages Haskell dependencies independently using `stack.yaml`.
When developing with Nix and Stack, it's up to you to make sure the versions
used by Stack are congruent to those used by Nix.


# Exploring the code

The best way to explore this project is to just jump into the source code.
Here's a roadmap:

| File/Directory               | Description                                 |
| -----------------------------| ------------------------------------------- |
| build.nix                    | root-level project configuration            |
| cabal.project                | `cabal` "new-*" multi-project configuration |
| default.nix                  | `nix-build` default configuration           |
| modules/app                  | Haskell "example-app" application using "example-lib" library |
| modules/nix/call/overrides   | default overrides for `nixpkgs`             |
| modules/nix/call/default.nix | main Nix library for project                |
| modules/lib                  | Haskell "example-lib" library               |
| modules/stack                | Nix expressions for Stack integration       |
| modules/ekg-assets           | Assets stripped from `ekg` Nix derivation   |
| modules/bundle               | Minimization of "example-app"               |
| shell.nix                    | `nix-shell` default configuration           |
| stack.yaml                   | Haskell `stack` default configuration       |

Most of the important Nix code is in `modules/nix/call/default.nix`.


# Hacks

There are three hacks in this project that would be wonderful to improve:

- environment merging for `nix-shell`
- automation of cabal2nix is fragile
- `replace-literal` /nix/store reference replacement (for minimization)

## environment merging

The Haskell-specific `mkDerivation` function provided by `nixpkgs` has an
"env" attribute intended for use with `nix-shell`.  This works fine with older
usage of `cabal`, which historically builds projects one-at-a-time.

However, with the recent "new-*" functionality in `cabal`, we can build
multiple Haskell projects with one "new-build" command.  So for `nix-shell`
we'd want an environment merged from all the environments we want to build.

This project does this (makes a `nix-shell` environment for all the
dependencies in `build.nix`), but the function was reverse engineered and may
be fragile.  Please submit improvements if you know of any.

## fragile cabal2nix automation

Because we use cabal2nix to build a derivation, which we then import in the
same run of `nix-build`, we're essentially bolting on a macroing system into
Nix, which can cause [some problems](https://github.com/NixOS/nix/issues/1148).
Most of these problems relate to caching in /nix/store.

However, for the simple case of just getting a build to work, the technique
works as you would expect.

If you really want to manually create `default.nix` files in your Haskell
projects with `cabal2nix`, you can.  If a `default.nix` is found, it will be
used instead by this project.

## replace-literal

This project intentionally uses `ekg` because it introduces an interesting
problem.  To prune references into `/nix/store`, we're statically compiling
our application and copying it into the "bundle" module.

But `ekg` uses Cabal's "data-files" feature, which means that upon compilation,
the binary ends up with hard-coded references back into `/nix/store`,
specifically to point to web assets like HTML and Javascript files.  These
references contain the exact shared objects that we were trying to avoid
referencing by statically compiling in the first place, which means the
transitive closure of required derivations in `/nix/store` grows substantially.

Our hack is to pluck out the web assets from `ekg` into a separate Nix
derivation called `ekg-assets`, and to then _carefully_ replace the reference
in the statically compiled `example-app` binary.  This is done in the "bundle"
module's builder script using the  `replace-literal` tool from `nixpkgs`.

As mentioned in
[Gabriel439/haskell-nix#12](https://github.com/Gabriel439/haskell-nix/issues/12)
and [NixOS/nixpkgs#4504](https://github.com/NixOS/nixpkgs/issues/4504), there
may be a better long-term solution (yet unimplemented).  This is a stopgap
solution we can use now.
