# Example Nix Project

This project illustrates using [the Nix package manager][nix] for programming
heterogeneous, multi-module projects.  It currently illustrates
only [Haskell][haskell] development, but can be extended for other language
platforms.

Use this project to learn and explore what's possible.  It's set up more for
pedagogy, but you should be able to use this code in real projects (I do).
There are many ways to organize code, and the beauty of Nix is that it's very
flexible.  So use this project if you can, but be aware that there's many ways
to achieve the same goal, and your needs will determine what's best.

I tried to make the code in this repository clean and self-documenting.  If you
have a way to improve its clarity, please submit an issue or pull request.
Otherwise, feel free to fork/modify it.

If you're extremely eager to get started, you can skip to the
[Quick Start](#quick-start) section.


## Motivation

Every programming language ecosystem has its particular set of tools for
building and managing libraries.  Reaching for a tool like the Nix package
manager may at first seem redundant.

However many platforms have FFI wrapper libraries over native C libraries.  Or
they require external dependencies for compilation or runtime.  Furthermore,
some domains like machine learning require the integration of different
applications developed in languages like C/C++, R, or Python.

To maintain reproducibility while integrating such heterogeneous builds, we
could use something like Docker.  But most of these solutions involve opaque
artifacts that are hard to trust.  For instance, when we use a "debian:8.7"
image from Docker Hub, it's not clear how to reproduce it if we have to.  We
just trust the community to have created a reasonable image which is then
frozen with a version identifier "8.7" and published on Docker Hub.

The Nix package manager provides a better architecture for reproducible builds
by treating the build process as a mathematical function.  In Nix, these
functions are written in a language also called Nix.  And as in math, Nix
expressions yield the same result, even when called at different times.

This allows us to reference and cache results with far more precision and
resolution than we can with Docker's Dockerfiles (which have little guarantee
of building the same Docker image if called twice on separate machines).  The
Nix ecosystem even goes further by patching compilers to make compiled
artifacts bit-for-bit reproducible.  And because Nix is based on mathematic
functions, there's lots of composition (as in f°g), which you can use to mix
your C, R, Haskell, or whatever.

Central to Nix is a special Git repository
called [Nix Packages (Nixpkgs)][nixpkgs], which is a large tree of Nix
expressions for all kinds of tools and libraries built from a variety of source
languages/platforms (enough to support [an entire operating system][nixos]).
These tools and libraries are defined in Nixpkgs with expressions that
evaluate to a type of value called a "derivation."  When instantiated, each
derivation is stored into a directory called `/nix/store`.

As [Gabriel Gonzalez points out][gonzalez-critique], Nix is not without its
problems.  Nix could use more documentation and tools to ease adoption.  Also
it takes a lot of work to curate all of Nixpkgs, so you may occasionally find
yourself writing your own Nix expressions, or contributing back to the project.

For many of us the benefits of Nix clearly outweigh the inconveniences.
Hopefully, projects and tutorials like this can help tip the balance further.


## Prerequisites


### Knowledge

Because this project is specialized for Haskell currently, it may help to have
some familiarity with Haskell development (for instance, know
what [Cabal][cabal] is and how to use it).

Also, this project is no substitute for official documentation for Nix:

- [Nix Manual](http://nixos.org/nix/manual/)
- [Nixpkgs Manual](http://nixos.org/nixpkgs/manual/)

There are also [many good tutorials][nix-tutorials].  Here are few that I found
useful personally:

- [Gabriel Gonzalez's haskell-nix][haskell-nix]
- [John Wiegley's Haskell/Nix talk][wiegley]
- [Luca Bruno's "pill" series][nix-pills]

You can definitely explore this project without diving into these resources
first, but if you have questions, it's good to have them as references.


### Technology

At a minimum, you should have [the Nix package manager installed][nix].  Unless
you're running NixOS as your operating system (which provides Nix
intrinsically) the official way to install Nix on another operating system is
by running Nix's install script:

```
$ curl https://nixos.org/nix/install | sh
```

Also, be aware that this "example-nix" project is actively tested on a
GNU/Linux OS, and only loosely tested on Macs.  Nix support for Macs is always
improving, but is known to be idiosyncratic.

You don't need to install the following tools because they are provided through
Nix:

- [Glasgow Haskell Compiler (GHC)][ghc]
- [Cabal][cabal]
- [Ghcid][ghcid].

However, external to this project, you may optionally use (discussed in this
document):

- [Docker][docker]
- [the Emacs text editor][emacs]
- [Dante (for Emacs and Haskell)][dante]
- [Haskell Stack][stack].


## Features

Here's some language-agnostic features of this project:

- supports mixed platform build (C, Haskell, almost anything else) via Nix
- managed concisely in a ["call-package"][callpackage]-style in
  [one file](./build.nix)
    - pins [Nixpkgs][nixpkgs] to a specified version for deterministic builds
    - shows how to override individual packages in the pinned set
- organized into modules (Nix expressions get twisted easily)
- shows how to integrate into Docker
- can generate a license report for many runtime dependencies
- supports typical Nix `nix-build`, `nix-env`, and `nix-shell` calls
- supports ignoring files
    - avoids needless rebuilds
    - keeps ignored files out of `/nix/store`.

Here's some features specific to Haskell:

- automatically calls [Cabal2nix][cabal2nix] for us (Haskell projects are
  Nix-free)
- supports statically linked binaries with a compact dependency set
- illustrates workaround to keep dependencies compact even if they use Cabal
  "data-files" (using [`replace-literal`](#replace-literal))
- provides recursive [Ctags/Etags][ctags] generation (using
  `nix-shell`)
- supports [Cabal][cabal] ["new-\*" builds][cabal-new] (using `nix-shell`)
- supports [Ghcid][ghcid] (using `nix-shell`)
- supports [Haskell Stack][stack]
- supports [Dante](https://github.com/jyp/dante) (an [Intero][intero] fork)
  for [Emacs][emacs] integration without Stack.


## Quick Start

To avoid over-explaining everything in this project, let's see it in action.


### Build, run, and install

If you have this repository cloned and Nix installed, you should be able to
build it with a simple call to `nix-build` from the project's root:

```
example-nix$ nix-build
fetching path ‘/nix/store/30mmlxk1lxiilmwvmalv46yx7rpbvbsb-mirrors-list’...
fetching path ‘/nix/store/8b7xhx2cq287xv5wd65cwxggkgzq8cxs-curl-7.53.0-man’...
fetching path ‘/nix/store/89znip2gs7c97027q7031ikfx7yvjyc8-nghttp2-1.19.0’...
fetching path ‘/nix/store/2hkx5jq0asymzc2h4fqcm7xx265fd6sn-unzip-6.0’...
...
shrinking RPATHs of ELF executables and libraries in /nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0
shrinking /nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0/bin/example-app
stripping (with flags -S) in /nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0/bin
patching script interpreter paths in /nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0
/nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0
```

The `nix-build` invocation without any arguments uses the Nix expression in
[./default.nix](./default.nix).  This resultant build is stored in
`/nix/store`, but `nix-build` leaves behind "result" symlink in the current
directory for our convenience:

```
result -> /nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0
└── bin
    └── example-app
```

You can run this application:

```
example-nix$ result/bin/example-app
EKG running on http://localhost:8081
hit any key to quit
```

The example application is written in Haskell and runs EKG, a monitoring
service, which you can see running using the URL reported.  When you hit a key,
it stops.

If you like, you can install the application into your Nix profile, so it's on
your shell's path (rather than having to call it through the "result" symlink):

```
example-nix$ nix-env --install --file .
installing ‘example-app’

example-nix$ which example-app
~/.nix-profile/bin/example-app
```

And if you no longer want it in your profile, you can uninstall it with
`nix-env` as well:

```
example-nix$ nix-env --uninstall example-app
uninstalling ‘example-app’

example-nix$ which example-app
example-app not found
```


### Compacting build dependencies

The version of `example-app` that we've compiled has a mixture of
dynamic and static linking.  When dynamically linking, Nix by nature hardcodes
paths back into `/nix/store`.  This is very important, because it allows for
different compilations to rely on different versions of dependencies without
conflicts.

We can see the transitive closure of all these dependencies with `nix-store`:

```
example-nix$ nix-store --query --requisites result | xargs du -sh | sort -h
48K	/nix/store/wb3mjqsigi5vnaz4dq8g5bzayj08bcaq-bytestring-builder-0.10.8.1.0
124K	/nix/store/sc5cjkp0mskc18lyfvh4pw00rhas2yqq-zlib-1.2.11
124K	/nix/store/xwfq79zv12ii6sirww9ahmcad1jm0s7d-gcc-wrapper-5.4.0
...
115M	/nix/store/ws967rii65nbzvabprpr3k6lpm744n6s-gcc-5.4.0
175M	/nix/store/qihggqjh7xb34czwbi1fldy0lyvj661b-ghc-8.0.2-doc
1019M	/nix/store/82khn09zw1dix9c22aigkgyimkk5wn1c-ghc-8.0.2
```

As you can see, `example-app` as compiled pulls dependencies that require a lot
of space.  This can be a serious impedance if we want to package/distribute a
runnable version of our application (lost time to network uploads/downloads,
hitting disk quotas, as so forth).

It turns out, even when statically linking, EKG has a quirk that pulls in
lots of dependencies (which is why it was intentionally chosen for our example
application).  Fortunately, this project
illustrates [a technique](#replace-literal) to address this problem.

If you look at [./default.nix](./default.nix), you'll see that it references
the "example-app-static" attribute in a set defined
in [./build.nix](./build.nix#L61-L71).

We could have built this explicitly with `nix-build`:

```
example-nix$ nix-build --attr example-app-static ./build.nix
/nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0
```

Notice it didn't build anything, because it used the cached result from
earlier.

The "example-app-compact" attribute references a dependency-compacted version
of our application, which we can build and run similarly:

```
example-nix$ nix-build --attr example-app-compact ./build.nix
these derivations will be built:
  /nix/store/cfwail2mjpmjdziiwh27zynjpwl268lz-ekg-assets.drv
  /nix/store/lcrwrcvy5pi91g621ahk06nh57jfqfd5-example-compact.drv
...
building path(s) ‘/nix/store/6rqp796l9mkckk5v8cvszh5m4fb98z2k-example-compact’
prepatched GBN: /nix/store/19r0x29297f8gg4j8n7q76g0fyh2l5vc-example-app-0.1.0.0/bin/example-app
   patched GBN: /nix/store/6rqp796l9mkckk5v8cvszh5m4fb98z2k-example-compact/bin/example-app
Scanning binary file /nix/store/6rqp796l9mkckk5v8cvszh5m4fb98z2k-example-compact/bin/example-app (2788048 bytes)...
"/nix/store/zzyyxa5xrc0qwhndkrrr93flz4jrc0k0-ekg-0.4.0.12" ->  "/nix/store/6hisvjm8z30bx8isijpz81pjimkm4j3g-ekg-assets\0" (offset: 2371464)
The binary file /nix/store/6rqp796l9mkckk5v8cvszh5m4fb98z2k-example-compact/bin/example-app would have had one string replaced
/nix/store/6rqp796l9mkckk5v8cvszh5m4fb98z2k-example-compact
```

This replaced our "result" symlink with another one.  This time, when we look
at the transitive closure of our dependencies, we see a much smaller and
compact set:

```
example-nix$ nix-store --query --requisites result | xargs du -sh
124K	/nix/store/sc5cjkp0mskc18lyfvh4pw00rhas2yqq-zlib-1.2.11
220K	/nix/store/6hisvjm8z30bx8isijpz81pjimkm4j3g-ekg-assets
684K	/nix/store/qz0jab95anfiycjy99s92svi4y2w3cv1-gmp-6.1.1
2.7M	/nix/store/6rqp796l9mkckk5v8cvszh5m4fb98z2k-example-compact
3.4M	/nix/store/b9nf47v92wm21vjr1yids22wqgwykxbw-openssl-1.0.2k
4.7M	/nix/store/blfi57w6szqpq21fkzlc08vyp5dz0ajk-gcc-5.4.0-lib
23M	/nix/store/a5gvhlwrday3dj8z3v09nr65ngn5jzq3-glibc-2.25
```

### Putting the app in a Docker container

The "example-tarball" attribute of the Nix expression
in [./build.nix](./build.nix) has an expression that takes the transitive
closure of `example-app-compact` and puts it into a tarball:

```
example-nix$ nix-build --attr example-tarball ./build.nix
these derivations will be built:
  /nix/store/x2gwykmcif2qska0w8wcy947v3vbvk9i-runtime-deps.drv
  /nix/store/5mq3b5xs8qs0ssk63cf04xa9ydsiyxmj-example-compact.tar.gz.drv
...
/nix/store/5a1zwdaikp42p7l6f8ylq21g68byfjca-example-compact.tar.gz
```

Again, the "result" symlink has changed, and now it points to the tarball we've
built.

We can use Docker to unpack this tarball into a "scratch" Docker image, and
then run it.  If you're running Linux, this works fine.  But if you built the
example application on a Mac, the built binary will be incompatible with
Docker.

In [./bin](./bin) there's two scripts:

- [build\_native-package\_docker](./bin/build_native-package_docker) (for
  Linux only)
- [build\_docker-package\_docker](./bin/build_docker-package_docker) (for
  Macs and Linux)

If you have Docker installed, these scripts can be run from a fresh checkout,
and illustrate everything from building `example-tarball` to putting it in a
Docker image, and finally running it:


```
$ ./bin/build_docker-package_docker
/nix/store/5a1zwdaikp42p7l6f8ylq21g68byfjca-example-compact.tar.gz
Sending build context to Docker daemon 15.09 MB
Step 1/4 : FROM scratch
 --->
Step 2/4 : ADD example-app.tar.gz /
 ---> Using cache
 ---> 82e78eac00c0
Step 3/4 : EXPOSE 8081
 ---> Using cache
 ---> 283504517d1c
Step 4/4 : ENTRYPOINT /bin/example-app
 ---> Using cache
 ---> f99fff0bdcc6
Successfully built f99fff0bdcc6
EKG running on http://localhost:8081
hit any key to quit
```

What the Mac-friendly script does differently is run `nix-build` from within a
Docker container, so the artifact is built for Linux instead of a Mac.

Notice the Docker image produced is fairly small (under 35MB):

```
example-nix$ docker images nix-example-app
REPOSITORY       TAG     IMAGE ID      CREATED         SIZE
nix-example-app  latest  a40ab024f5c1  16 seconds ago  32.5 MB
```


### Making a runtime license report

Figuring out whether an application is properly licensed requires going through
all the licenses of all the dependencies used.

Unfortunately, Nix doesn't offer a complete solution for this, but can help a
little.

The "example-extra.licenses" attribute of the Nix expression
in [./build.nix](./build.nix) has an expression that generates a JSON file with
license information for many of the dependencies required in `/nix/store`.

There are two important omissions in this generated report:

- no mention of statically compiled libraries
- some dependencies have missing license information

When a library is statically compiled, Nix loses track of the dependency.  To
get a more accurate license report, create it from a dynamically-generated
variant instead.  The report generated by this project includes results for a
dynamically linked version of `example-app` and `example-app-compact`
(statically linked and compacted) so you can see the difference.

Additionally, you may notice a project listed as a dependency with no license
information.  This is
a [limitation of current state of the art in Nix](#incomplete-license-report).

The [bin/licenses](./bin/licenses) script builds this JSON license report, and
as a convenience runs it through the `jq` tool for formatting, and then through
`less` for paging:

```
example-nix$ bin/licenses
{
  "example-app-dynamic": [
    {
      "homepage": "https://github.com/basvandijk/monad-control",
      "license": {
        "fullName": "BSD 3-clause \"New\" or \"Revised\" License",
        "shortName": "bsd3",
        "spdxId": "BSD-3-Clause",
        "url": "http://spdx.org/licenses/BSD-3-Clause"
      },
      "path": "/nix/store/h5a17magy1p7lv14l9zk5pgpl084vrb7-monad-control-1.0.1.0"
    },
    ...
```


## Developing Haskell


### Development tools in a Nix shell

This project also provides a Nix expression for `nix-shell` that creates a
developer environment containing:

- `ghc`, preloaded with all your dependencies
- `cabal-install`
- `ghcid`
- `nix-tags-haskell`
- `cabal-new-watch`

We can enter this environment by calling `nix-shell` with no arguments (which
by default reads [./shell.nix](./shell.nix).  Notice how the environment gives
us a PATH including binaries in `/nix/store`:

```
example-nix$ nix-shell

[nix-shell:example-nix]$ which ghc
/nix/store/83yr2nkjhwi7zjb5sbv8m19qp0z7gd2x-ghc-8.0.2-with-packages/bin/ghc

[nix-shell:example-nix]$ which ghci
/nix/store/83yr2nkjhwi7zjb5sbv8m19qp0z7gd2x-ghc-8.0.2-with-packages/bin/ghci

[nix-shell:example-nix]$ which ghcid
/nix/store/83yr2nkjhwi7zjb5sbv8m19qp0z7gd2x-ghc-8.0.2-with-packages/bin/ghcid

[nix-shell:example-nix]$ which cabal
/nix/store/83yr2nkjhwi7zjb5sbv8m19qp0z7gd2x-ghc-8.0.2-with-packages/bin/cabal

[nix-shell:example-nix]$ which nix-tags-haskell
/nix/store/wffiii1cinvq9m6jnsxniryxng1my45j-nix-tags-haskell/bin/nix-tags-haskell

[nix-shell:example-nix]$ which cabal-new-watch
/nix/store/mfca92lxm1nh4sf1qn61gvn4qwim5bk1-cabal-new-watch/bin/cabal-new-watch

[nix-shell:example-nix]$ exit

example-nix$
```

The instance of GHC provided is a "-with-packages" version preloaded with all
the external libraries and tools declared as dependencies gleaned from our
project's Cabal files (`example-lib.cabal` and `example-app.cabal`).  This
includes only external libraries/tools like EKG, but excludes the
libraries/tools of this project like `example-lib` or `example-app`.  This way
versions of external dependencies are explicitly pinned to the versions coming
from Nixpkgs, and not resolved dynamically by Cabal.

One consequence of this is that once you enter into a Nix shell for a
derivation, you can disable your computer's networking.  Entering the shell
should download all the dependencies you need from the internet, and check
their hashes to assure a deterministic build.  From there, you should only need
your source code, which should should be able to compile and work with offline.


#### Cabal

When we built our project before with `nix-build` it used Cabal internally, but
we can use `nix-shell` to call Cabal explicitly.

The provided version of Cabal is new enough, we can use its
latest ["new-\*" support for multiple projects][cabal-new].

Also, we can use the "--run" switch of `nix-shell` to save us the hassle of
entering and exiting the environment:

```
example-nix$ nix-shell \
    --run 'cabal update; cabal new-configure; cabal new-build example-app'
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
and ready to run:

```
example-nix$ ./dist-newstyle/build/example-app-0.1.0.0/build/example-app/example-app
EKG running on http://localhost:8081
hit any key to quit
```

Note, these "new-\*" commands have been released as such for testing by the
Cabal development team.  Once they've been deemed stable, the normal
"configure/build/repl" commands will be replaced with their "new-"
counterparts, which will go away.

In an effort to help to contribute towards these commands' success, this
project does not shy from using them, and encourages people to try them out.

If you're familiar with Cabal sandboxes, you can use those too instead of the
"new-\*" commands, but you will deviate from the integration of tools
illustrated by this document.  Some things may break and require a different
approach, so sandboxes are beyond the scope of this project.  Also, Sandboxes
seem likely to go away with once the "new-\*" commands are officially released.


#### Accidentally building outside Nix shell

As discussed, the version of GHC we get in our Nix shell comes integrated with
all our project dependencies.  These are all built and installed into
`/nix/store` upon entering the shell.

Therefore, when using our Nix shell this way, we'll never have to compile and
install binaries into our local ~/.cabal directory, which should remain
spartan:

```
~/.cabal/packages
└── hackage.haskell.org
    ├── 00-index.cache
    ├── 00-index.tar
    ├── 00-index.tar.gz
    └── 00-index.tar.gz.etag
```

If you call cabal outside of the Nix shell you'll see dependencies download
from the internet and compile:

```
$ cabal new-build
Resolving dependencies...
Downloading HUnit-1.5.0.0...
Downloading aeson-1.1.0.0...
...
```

This build will not use Nix at all, and use Cabal to resolve dependencies.  In
Nix, we instead get dependencies from a specific set pinned
in Nixpkgs.

Of course, a simple way to avoid this problem altogether is to not install
Cabal or GHC outside Nix shell.

If you get confused about whether you've compiled with or without Nix, you can
always delete the following folders and try again:

- ~/.cabal/packages
- ~/.cabal/store
- ./dist-newstyle


#### Ghcid

From `nix-shell` you can run `ghcid`, which some people like for fast
incremental compilation while developing:

```
example-nix: nix-shell
[nix-shell:example-nix]$ cd modules/example-app
[nix-shell:app]$ ghcid --command 'cabal new-repl'
```

Ghcid will sense changes in source files, and automatically recompile them.
Later, we'll show how we can get a similar benefit from within Emacs using
Dante.

Note, the reason Ghcid is faster than a normal build with Cabal or Stack is
because, it's using a REPL session, which it uses to reload modules.  This
provides a faster compilation, but sometimes error messages get out of sync,
and you have to restart Ghcid.


#### Editor tags files

If you use a text editor like Emacs or Vim, you can navigate multiple projects
fluidly using [Ctags/Etags][ctags].  For Haskell, this project's Nix shell
environment provides a `nix-tags-haskell` script to create a tags file:

```
example-nix$ nix-shell --run nix-tags-haskell
...
Success
```

By default the Ctags-formatted file (used by Vim) is put in `./tags` and the
Etags file (used by Emacs) is put in `./TAGS`.  `nix-tags-haskell` provides
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

Note that Dante (like Ghcid) uses a REPL for faster compilation than a normal
Cabal or Stack build.  So like Ghcid, its errors can fall out of sync with a
true build, and you'll need to restart the session with `M-x dante-restart`.


#### Haskell Stack

If you have Stack installed, you can run it from the root project:

```
example-nix$ stack build --file-watch
...
example-lib-0.1.0.0: configure (lib)
example-lib-0.1.0.0: build (lib)
example-lib-0.1.0.0: copy/register
example-app-0.1.0.0: configure (exe)
example-app-0.1.0.0: build (exe)
example-app-0.1.0.0: copy/register
Completed 2 action(s).
Log files have been written to: example-nix/.stack-work/logs/
ExitSuccess
Type help for available commands. Press enter to force a rebuild.
```

Because we used the "--file-watch" switch, Stack will rebuild the project when
files change, similarly to Ghcid and Dante.

**WARNING**: This project uses
Stack's [built-in support for Nix integration][stack-nix].  System dependencies
for Stack come from the Nix configuration.  But be aware that Stack manages
Haskell dependencies independently using the "resolver" specified in
`./stack.yaml`.  When developing with Nix and Stack, it's up to you to make
sure the versions used by Stack are congruent to those used by Nix.

Use Dante or another method from this document if you want to avoid this
problem with Stack entirely.


#### Stack-less change-triggered builds

The `nix-shell` environment offered by this project provides a
`cabal-new-watch` script that emulates `stack build --file-watch` but only
using dependencies managed by Nix.

```
$ nix-shell --run 'cabal-new-watch example-app'
TRIGGER: first run
Up to date
Sun Feb 26 18:44:17 CST 2017: SUCCESS
```

This is a true Cabal build, so it won't be as fast as Ghcid or Dante, but
should be about as fast as a normal Stack build.

Cabal "new-\*" builds are a nice way to manage multi-module projects.
Unfortunately, there's
a [race-condition when concurrent builds are running][cabal-new-issue].  If you
run `cabal-new-watch` and Dante together, you may find your build gets
corrupted.  This is easy to fix by deleting the `./dist-newstyle` directory.

The Cabal team is aware of this issue, so hopefully this inconvenience will be
addressed sooner than later.


## Exploring the code

You've not reached the end of the tutorial, seen a what this project has to
offer, and learned how to use it.

The next step is to better learn the Nix expression language, and see how the
code works.  Here's a roadmap:

| File/Directory               | Description                                  |
| -----------------------------| -------------------------------------------- |
| bin/                         | scripts to illustrate this project           |
| build.nix                    | root-level project configuration             |
| cabal.project                | Cabal "new-\*" multi-project configuration   |
| default.nix                  | `nix-build` configuration                    |
| modules/ekg-assets           | assets stripped from EKG Nix derivation      |
| modules/example-app          | Haskell "example-app" application using "example-lib" library |
| modules/example-app-compact  | minimization of "example-app"                |
| modules/example-lib          | Haskell "example-lib" library                |
| modules/pkgs-make/           | the Nix expression driving this project      |
| modules/stack                | Nix expressions for Stack integration        |
| shell.nix                    | `nix-shell` configuration                    |
| stack.yaml                   | Stack configuration                          |

Most of the supporting Nix code is
in [./modules/pkgs-make/](./modules/pkgs-make), which is organized as follow:

| File/Directory               | Description                                  |
| -----------------------------| -------------------------------------------- |
| default.nix                  | entry point for pkg-make Nix expression      |
| haskell.nix                  | Haskell-specific Nix expressions             |
| tools/                       | more derivations of tools to extend Nixpkgs  |
| lib/                         | more Nix expressions to extend Nixpkgs       |
| overrides/                   | default overrides for Nixpkgs derivations    |


## Hacks

There are a few hacks in this project that would be wonderful to improve, but
are probably fine for most contexts:

- license report may often be incomplete
- automation of Cabal2nix is fragile
- `replace-literal` replacement of references in `nix-store` (for minimization)

The following discussion assumes some deeper knowledge of Nix and Nixpkgs.


### Incomplete license report

We find runtime dependencies by looking through the generated artifact in
`/nix/store`, and finding all further references within `/nix/store`.  This
doesn't tell us anything about compile-time dependencies.  Also, `/nix/store`
doesn't contain license information.

To match the detected dependencies with license information, we do a heuristic
crawl through Nixpkgs tree, starting with the derivation for our built
artifact.  Sometimes we don't find what we want.

Also, this license report is currently limited to runtime dependencies (which
is the common case for most inquiries).  Compile-time dependencies explodes to
a much larger set, and offers more challenges due to how some Nix expressions
use string-interpolation.

Note, the accuracy of the report is only as good as the information in
Nixpkgs.  For instance, `gmp` is currently listed as GPL-licensed, when it's
actually dual-licensed with both GPL and LGPL.

Hopefully this report is still useful, provided you understand the caveats.


### Fragile Cabal2nix automation

Because we use Cabal2nix to build a derivation, which we then import in the
same run of `nix-build`, we're essentially bolting on a macro system into Nix,
which can cause [some problems][cabal2nix-issue] Most of these problems relate
to caching in `/nix/store`.

However, for the simple case of just getting a build to work, the technique
works as you would expect.

If you really want to manually create `default.nix` files in your Haskell
projects with the `cabal2nix` command, you can.  If a `default.nix` is found,
it will be used instead by this project.


### Replace-literal

This project intentionally uses EKG because it introduces an interesting
problem.

To prune references into `/nix/store`, we're statically compiling our
application and copying it into the "example-app-compact" module.  This works
because shared objects and documentation often have references to other
dependencies not needed at runtime.

But EKG uses Cabal's "data-files" feature, which means that upon compilation,
the binary ends up with hard-coded references back into `/nix/store` where web
assets like HTML and Javascript files are stored.  However, these assets are
stored alongside the shared objects and documentation of EKG, which prevents
the transitive closure of dependencies from being compact.

Our hack is to pluck out the web assets from EKG into a separate Nix derivation
called "ekg-assets", and to then _carefully_ replace the reference in the
statically compiled "example-app" binary.  This is done in the
"example-app-compact" module's [builder script][replace-call] using the
`replace-literal` tool from Nixpkgs.

As mentioned in [Gabriel439/haskell-nix#12][haskell-nix-issue] and
[NixOS/nixpkgs#4504][nixpkgs-issue], there may be a better long-term solution
(yet unimplemented).  This is a stopgap solution we can use now.


[cabal2nix]: https://github.com/NixOS/cabal2nix
[cabal2nix-issue]: https://github.com/NixOS/nix/issues/1148
[cabal]: https://haskell.org/cabal
[cabal-new]: http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/
[cabal-new-issue]: https://github.com/haskell/cabal/issues/3741
[callpackage]: http://lethalman.blogspot.com/2014/09/nix-pill-13-callpackage-design-pattern.html
[ctags]: https://en.wikipedia.org/wiki/Ctags
[dante]: https://github.com/jyp/dante
[docker]: https://www.docker.com/products/overview
[emacs]: https://www.gnu.org/software/emacs/
[ghc]: https://www.haskell.org/ghc/
[ghcid]: https://github.com/ndmitchell/ghcid
[gonzalez-critique]: https://github.com/Gabriel439/haskell-nix#background
[haskell]: https://haskell.org
[haskell-nix]: https://github.com/Gabriel439/haskell-nix
[haskell-nix-issue]: https://github.com/Gabriel439/haskell-nix/issues/12
[intero]: https://github.com/commercialhaskell/intero
[nix]: https://nixos.org/nix
[nixos]: https://nixos.org/nixos
[nix-pills]: http://lethalman.blogspot.com/2014/07/nix-pill-1-why-you-should-give-it-try.html
[nixpkgs]: https://github.com/NixOS/nixpkgs
[nixpkgs-issue]: https://github.com/NixOS/nixpkgs/issues/4504
[nix-tutorials]: https://www.google.com/search?q=nix+tutorial
[projectile]: http://projectile.readthedocs.io
[replace-call]: ./modules/compact/builder.sh#L9-L10
[stack]: http://haskellstack.org
[stack-nix]: https://docs.haskellstack.org/en/stable/nix_integration/
[wiegley]: https://www.youtube.com/watch?v=G9yiJ7d5LeI
