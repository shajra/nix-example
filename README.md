# Example Nix Project

This project illustrates using [Nix][nix] for programming heterogeneous,
multi-module projects.  It's currently illustrates only [Haskell][haskell]
development, but can be extended for other language platforms.

Use this project to learn and explore what's possible.  It's set up more for
pedagogy, but you should be able to use this code in real projects.  There are
many ways to factor code, and the beauty of Nix is that it's very flexible.
So use this project if you can.  I tried to make the code in this repository
clean and self-documenting.  If you have a way to improve its clarity, please
submit an issue or pull request.  Otherwise, feel free to fork/modify it.

[nix]: https://nixos.org
[haskell]: https://haskell.org


## Motivation

Every language ecosystem has its particular set of tools for building and
managing libraries.  Reaching for a tool like Nix may at first seem redundant.

However many platforms have FFI wrapper libraries over native C libraries.  Or
they require external dependencies for compilation or runtime.  Furthermore,
some domains like machine learning require the integration of different
applications developed in languages like C/C++, R, or Python.

To maintain reproducibility while integrating such heterogeneous builds, we
could use something like Docker.  But most of these solutions involve opaque
artifacts that are hard to trust.  For instance, when we use a "debian:8.7"
image from DockerHub, it's not clear how to reproduce it if we have to.  We
just trust the community to have created a reasonable image which is then
frozen with a version identifier "8.7" and published on Dockerhub.

Nix provides a better architecture for reproducible builds by treating the
build process as a mathematical function.  In Nix, these functions are written
in a language called Nix expressions.  And as in math, Nix expressions yield
the same result, even when called at different times.

This allows us to cache results with far more precision and resolution than we
can with Docker's Dockerfiles (which have little guarantee of building the same
image if called twice on separate machines).  The Nix ecosystem even goes
further by patching compilers to make compiled artifacts bit-for-bit
reproducible.  And because Nix is based on mathematic functions, there's lots
of composition (as in f°g), which you can use to mix your C, R, Haskell, or
whatever.

As [Gabriel Gonzalez points out][gonzalez-critique], Nix is not without its
problems.  Nix could use more documentation and tools to ease adoption.
However, for some of us, the benefits of Nix clearly outweigh the
inconveniences.  Hopefully, projects and tutorials like this can help tip the
balance further.

[gonzalez-critique]: https://github.com/Gabriel439/haskell-nix#background


## Prerequisites


### Knowledge

Because this project is specialized for Haskell currently, it may help to have
some familiarity with Haskell development.

Also, this project is no substitute for official documentation for Nix:

- [Nix Manual](http://nixos.org/nix/manual/)
- [Nixpkgs Manual](http://nixos.org/nixpkgs/manual/)

There's also good tutorials:

- [Gabriel Gonzalez's nix-haskell][nix-haskell]
- [Luca Bruno's "pill" series][nix-pills]

You can definitely explore this project without diving into these resources
first, but if you have questions, it's good to have them as references.

[haskell-nix]: https://github.com/Gabriel439/haskell-nix
[nix-pills]: http://lethalman.blogspot.com/2014/07/nix-pill-1-why-you-should-give-it-try.html


### Technology

At a minimum, you should have [Nix installed][nix].

Also, though this project may work on Macs, it's only been tested within a
Linux OS.  Nix support for Macs is always improving, but is known to be
idiosyncratic.

You don't need to install the following tools because they are provided through
Nix:

- [Glasgow Haskell Compiler (GHC)][ghc]
- [Cabal][cabal]
- [Ghcid][ghcid].

However, external to this project, you may optionally use (discussed in this
document):

- [the Emacs text editor][emacs]
- [Dante (for Emacs)][dante]
- [Haskell Stack][stack].

[ghc]: https://www.haskell.org/ghc/


## Features

Here's some language-agnostic features of this project:

- supports mixed platform build (C, Haskell, almost anything else) via Nix
- managed concisely in a ["call-package"][callpackage]-style
  in [./build.nix](./build.nix)
    - pins [nixpkgs][nixpkgs] to a specified version for deterministic builds
      (`<nixpkgs>`/`NIX_PATH` only used for bootstrapping)
    - allows overriding of the pinned `nixpkgs`
    - comes with some baked-in overrides (just for illustration, you can
      change them)
- factored into modules (Nix expressions get twisted easily)
- supports typical Nix `nix-build`, `nix-env`, and `nix-shell` calls
- supports ignoring files
    - avoids needless rebuilds
    - keeps ignored files out of `/nix/store`.

Here's some features specific to Haskell:

- automatically calls [cabal2nix][cabal2nix] for us (Haskell projects are
  Nix-free)
- supports statically linked binaries with a compact dependency set
- illustrates workaround to keep dependencies compact even if they use Cabal
  "data-files" (using `replace-literal`)
- provides recursive [ctags/etags](https://ctags.io) generation (using
  `nix-shell`)
- supports [Cabal][cabal] ["new-\*" builds][cabal-new] (using `nix-shell`)
- supports [Ghcid][ghcid] (using `nix-shell`)
- supports [Haskell Stack][stack]
- supports [Dante](https://github.com/jyp/dante) (an [Intero][intero] fork)
  for [Emacs][emacs] integration without Stack.

[callpackage]: http://lethalman.blogspot.com/2014/09/nix-pill-13-callpackage-design-pattern.html
[cabal2nix]: https://github.com/NixOS/cabal2nix
[nixpkgs]: https://github.com/NixOS/nixpkgs
[cabal]: https://haskell.org/cabal
[ghcid]: https://github.com/ndmitchell/ghcid
[stack]: http://haskellstack.org
[cabal-new]: http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/
[dante]: https://github.com/jyp/dante
[intero]: https://github.com/commercialhaskell/intero
[emacs]: https://www.gnu.org/software/emacs/


## Quick Start

To avoid over-explaining everything in this project, let's first see it in
action.

If you have this repository cloned and Nix installed, you should be able to
build it with a simple call to `nix-build` from the project's root:


### Build, run, and install

```
example-nix$ nix-build
fetching path ‘/nix/store/48f5qyygrikrvja43i493sjmskzcpfjj-mirrors-list’...
fetching path ‘/nix/store/xlap4dwqfgjm581j07ww0ygavfmzixqi-curl-7.52.1-man’...
fetching path ‘/nix/store/nqg4ngyawb9w02r06f4n6nbqwfbrd7cg-gawk-4.1.3’...
fetching path ‘/nix/store/mypmpnb3m5bcg1q70gynpz05sr8czi16-gcc-5.4.0-man’...
...
building path(s) ‘/nix/store/rwagjvq6mxv6a0rcxzfkavmrx3bxwgxb-example-bundle’
prepatched GBN: /nix/store/l8wjk7mi2h4wlqbqmrf2gnmwz6rbc5rx-example-app-0.1.0.0/bin/example-app
   patched GBN: /nix/store/rwagjvq6mxv6a0rcxzfkavmrx3bxwgxb-example-bundle/bin/example-app
Scanning binary file
    /nix/store/rwagjvq6mxv6a0rcxzfkavmrx3bxwgxb-example-bundle/bin/example-app
    (2748880 bytes)...
"/nix/store/3ha5x9502a93j0n3a015bz0xz7rjnym6-ekg-0.4.0.12"
    -> "/nix/store/crn26c4canjpjbm6cxq2kb1z4i1q8y60-ekg-assets\0" (offset: 2324272)
The binary file
    /nix/store/rwagjvq6mxv6a0rcxzfkavmrx3bxwgxb-example-bundle/bin/example-app
    would have had one string replaced
/nix/store/rwagjvq6mxv6a0rcxzfkavmrx3bxwgxb-example-bundle
```

The `nix-build` invocation without any arguments uses the Nix expression in
[./default.nix](./default.nix).  This resultant build is stored in
`/nix/store`, but `nix-build` leaves behind "result" symlink in the current
directory for our convenience:

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

The example application runs EKG, a Haskell monitoring service, which you can
see running using the URL reported.  When you hit a key, it stops.

The application is statically linked, and the transitively closed set of
references back into `/nix/store` are minimal:

```
example-nix$ nix-store --query --requisites result | xargs du -sh
23M 	/nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24
4.7M	/nix/store/jar52969wyf10sh2wj62ipfjiw7xaq2j-gcc-5.4.0-lib
684K	/nix/store/a9wlwd8cfzyprwwkib5ibi3cn56v165y-gmp-6.1.1
220K	/nix/store/crn26c4canjpjbm6cxq2kb1z4i1q8y60-ekg-assets
124K	/nix/store/gdw5qz99ihbv3l3nc91jdynfm3va46qq-zlib-1.2.10
3.4M	/nix/store/nxcny3iwkq6bn0y16l2m6jbrblwnwmrl-openssl-1.0.2j
2.7M	/nix/store/8p8k06039cya3rylh8swx2jwk5146psh-example-bundle
```

If you like, you can install the application into your Nix profile, so it's on
your shell's path (rather than having to call it through the "result"
symlink):

```
example-nix$ nix-env --install --file .
installing ‘example-bundle’

example-nix$ which example-app
~/.nix-profile/bin/example-app
```

And if you no longer want it in your profile, you can uninstall it with
`nix-env` as well:

```
example-nix$ nix-env --uninstall example-bundle
uninstalling ‘example-bundle’

example-nix$ which example-app
example-app not found
```


### Development tools in a Nix shell

This project also provides a Nix expression for `nix-shell` that creates a
developer environment containing:

- `ghc`, preloaded with all your dependencies
- `cabal-install`
- `ghcid`
- `nix-tags-haskell`

We can either enter this environment by calling `nix-shell` with no arguments
(which by default reads [./shell.nix](./shell.nix).  Notice how the environment
gives us a PATH including binaries in `/nix/store`:

```
example-nix$ nix-shell

[nix-shell:example-nix]$ which ghc
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/ghc

[nix-shell:example-nix]$ which ghci
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/ghci

[nix-shell:example-nix]$ which ghcid
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/ghcid

[nix-shell:example-nix]$ which cabal
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/cabal

[nix-shell:example-nix]$ which nix-tags-haskell
/nix/store/956m7wcp0lqwgcrzz2h718fyz2ly76sx-nix-tags-haskell/bin/nix-tags-haskell

[nix-shell:example-nix]$ exit

example-nix$
```

The instance of GHC provided is a "-with-packages" version preloaded with all
the libraries and tools declared as dependencies in the various Cabal
"\*.cabal" files (like EKG), but excluding the dependencies built by the
project itself (like `example-lib` or `example-app`).  This way versions of
dependencies are explicitly pinned to the versions coming from `nixpkgs`, and
not resolved dynamically by Cabal.


#### Cabal

When we built our project with `nix-build` before it used Cabal internally, but
we can use `nix-shell` to call Cabal explicitly.

The provided version of Cabal is new enough, we can use its latest "new-\*"
support for multiple projects.

Also, we can use the "--run" switch of `nix-shell` to save us the hassle of
entering and exiting the environment:

```
example-nix$ nix-shell \
    --run 'cabal update; cabal new-configure; cabal new-build'
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

The last line shows where your Cabal-built binary can be found.  It's linked
and ready to run.


#### Ghcid

From `nix-shell` you can run `ghcid`, which some people like for fast
incremental compilation while developing:

```
example-nix: nix-shell
[nix-shell:example-nix]$ cd modules/app
[nix-shell:app]$ ghcid --command 'cabal new-repl'
```

Ghcid will sense changes in source files, and automatically recompile them.
Later, we'll show how we can get a similar benefit from within Emacs using
Dante.


#### Editor tags files

If you use a text editor like Emacs of Vim, you can navigate multiple projects
fluidly using ctags/etags.  For Haskell, we can generate tags from within the
Nix shell using a provided `nix-tags-haskell` script:

```
example-nix$ nix-shell --run nix-tags-haskell
...
Success
```

By default the ctags-formated file (used by Vim) is put in `./tags` and the
etags file (used by Emacs) is put in `./TAGS`.  `nix-tags-haskell` provides
some additional configuration you can see with the "--help" switch:

```
example-nix$ nix-shell --run 'nix-tags-haskell --help'

Synopsis: ...
```

In Vim, you can now use `Ctrl-]` and `Ctrl-t` to jump to declarations, even in
the source code for third-party projects and the standard/base libraries.  In
Emacs, you can use the `find-tag` command, which is by default bound to
`Meta-.`, and you can go back with `pop-tag-mark`, bound by default to
`Meta-*`.

There are also a myriad of ways to configure various editors to run the
tags-generation command on demand and/or as necessary.


#### Dante

If you're using Emacs, you can use [Dante][dante] as an alternative to Ghcid
and Stack.  Follow the official directions to install Dante in Emacs.  To make
Dante work with this project, you need to include a new entry in the
`dante-repl-command-line-methods-alist`.  Here's an illustration of how to do
this using the popular [Projectile][projectile] Emacs package to determine the
project's root directory (Projectile provides much more, though):

```lisp
(setq-default
   dante-repl-command-line-methods-alist
       `(
            (styx .
                ,(lambda (root)
                    (dante-repl-by-file root "styx.yaml"
                        '("styx" "repl"))))
            (nix-new .
                ,(lambda (root)
                    (dante-repl-by-file
                        (projectile-project-root)
                        "shell.nix"
                        `("nix-shell" "--run" "cabal new-repl"
                            ,(concat (projectile-project-root) "/shell.nix")))))
            (stack .
                ,(lambda (root)
                    (dante-repl-by-file root "stack.yaml"
                        '("stack" "repl"))))
            (bare  . ,(lambda (_) '("cabal" "repl")))))
```

The important part for this project is the "nix-new" entry.  The other entries
are so Dante will continue to work with other types of Haskell projects.

Emacs configured with tags and Dante provides an extremely rich "IDE"-like
developer experience for Haskell.

[projectile]: http://projectile.readthedocs.io


#### Haskell Stack

Finally, if you have Stack installed, you can run it from the root project:

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

**WARNING**: System dependencies for Stack come from the Nix configuration,
but be aware that Stack manages Haskell dependencies independently using
`./stack.yaml`.  When developing with Nix and Stack, it's up to you to make
sure the versions used by Stack are congruent to those used by Nix.

Use Dante if you want to avoid this problem with Stack entirely.


## Exploring the code

The best way to explore this project is to jump into the source code.
Here's a roadmap:

| File/Directory               | Description                                  |
| -----------------------------| -------------------------------------------- |
| build.nix                    | root-level project configuration             |
| cabal.project                | `cabal` "new-\*" multi-project configuration |
| default.nix                  | `nix-build` configuration                    |
| modules/app                  | Haskell "example-app" application using "example-lib" library |
| modules/nix/default.nix      | main Nix library for project                 |
| modules/nix/overrides        | default overrides for `nixpkgs`              |
| modules/nix/ctags            | nix-tags-haskell script                      |
| modules/lib                  | Haskell "example-lib" library                |
| modules/stack                | Nix expressions for Stack integration        |
| modules/ekg-assets           | Assets stripped from `ekg` Nix derivation    |
| modules/bundle               | Minimization of "example-app"                |
| shell.nix                    | `nix-shell` configuration                    |
| stack.yaml                   | `stack` configuration                        |

Most of the implementation Nix code is in
[./modules/nix/default.nix](./modules/nix/default.nix).


## Hacks

There are three hacks in this project that would be wonderful to improve, but
are probably fine for most contexts:

- environment merging for `nix-shell`
- automation of cabal2nix is fragile
- `replace-literal` /nix/store reference replacement (for minimization)


### Environment merging

The Haskell-specific `mkDerivation` function provided by `nixpkgs` has an
"env" attribute intended for use with `nix-shell`.  This works fine with older
usage of `cabal`, which historically builds projects one-at-a-time.

However, with the recent "new-\*" functionality in `cabal`, we can build
multiple Haskell projects with one "new-build" command.  So for `nix-shell`
we'd want an environment merged from all the environments we want to build.

This project does this (makes a `nix-shell` environment for all the
dependencies in `build.nix`), but the function was reverse engineered and may
be fragile.  Please submit improvements if you know of any.


### Fragile cabal2nix automation

Because we use cabal2nix to build a derivation, which we then import in the
same run of `nix-build`, we're essentially bolting on a macroing system into
Nix, which can cause [some problems][cabal2nix-issue] Most of these problems
relate to caching in /nix/store.

However, for the simple case of just getting a build to work, the technique
works as you would expect.

If you really want to manually create `default.nix` files in your Haskell
projects with `cabal2nix`, you can.  If a `default.nix` is found, it will be
used instead by this project.

[cabal2nix-issue]: https://github.com/NixOS/nix/issues/1148


### Replace-literal

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
module's [builder script][replace-call] using the  `replace-literal` tool from
`nixpkgs`.

As mentioned in [Gabriel439/haskell-nix#12][haskell-nix-issue] and
[NixOS/nixpkgs#4504][nixpkgs-issue], there may be a better long-term solution
(yet unimplemented).  This is a stopgap solution we can use now.

[replace-call]: ./modules/bundle/builder.sh#L9-L10
[haskell-nix-issue]: https://github.com/Gabriel439/haskell-nix/issues/12
[nixpkgs-issue]: https://github.com/NixOS/nixpkgs/issues/4504
