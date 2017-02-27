# Example Nix Project

This project illustrates using [Nix][nix] for programming heterogeneous,
multi-module projects.  It currently illustrates only [Haskell][haskell]
development, but can be extended for other language platforms.

Use this project to learn and explore what's possible.  It's set up more for
pedagogy, but you should be able to use this code in real projects (I do).
There are many ways to organize code, and the beauty of Nix is that it's very
flexible.  So use this project if you can.  I tried to make the code in this
repository clean and self-documenting.  If you have a way to improve its
clarity, please submit an issue or pull request.  Otherwise, feel free to
fork/modify it.

If you're extremely eager to get started, you can skip to the
[Quick Start](#quick-start) section.


## Motivation

Every programming language ecosystem has its particular set of tools for
building and managing libraries.  Reaching for a tool like Nix may at first
seem redundant.

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

This allows us to reference and cache results with far more precision and
resolution than we can with Docker's Dockerfiles (which have little guarantee
of building the same Docker image if called twice on separate machines).  The
Nix ecosystem even goes further by patching compilers to make compiled
artifacts bit-for-bit reproducible.  And because Nix is based on mathematic
functions, there's lots of composition (as in f°g), which you can use to mix
your C, R, Haskell, or whatever.

Central to Nix is a special Git repository called [`nixpkgs`][nixpkgs], which
is a large tree of Nix expressions for all kinds of tools and libraries built
from a variety of source languages/platforms (enough to
support [an entire operating system][nixos]).

As [Gabriel Gonzalez points out][gonzalez-critique], Nix is not without its
problems.  Nix could use more documentation and tools to ease adoption.  Also
it takes a lot of work to curate all of `nixpkgs`, so you often find yourself
writing your own, or contributing back to the project.

For many of us the benefits of Nix clearly outweigh the inconveniences.
Hopefully, projects and tutorials like this can help tip the balance further.


## Prerequisites


### Knowledge

Because this project is specialized for Haskell currently, it may help to have
some familiarity with Haskell development (for instance, know
what [Cabal][cabal] is).

Also, this project is no substitute for official documentation for Nix:

- [Nix Manual](http://nixos.org/nix/manual/)
- [Nixpkgs Manual](http://nixos.org/nixpkgs/manual/)

There are also good tutorials:

- [Gabriel Gonzalez's haskell-nix][haskell-nix]
- [Luca Bruno's "pill" series][nix-pills]

You can definitely explore this project without diving into these resources
first, but if you have questions, it's good to have them as references.


### Technology

At a minimum, you should have [Nix installed][nix].  Unless you're running
NixOS as your operating system (which provides Nix intrinsically) the official
way to install Nix on another operating system is by running their install
script:

```
$ curl https://nixos.org/nix/install | sh
```

Also, be aware that his project is actively tested on a GNU/Linux OS, and only
loosely tested on Macs.  Nix support for Macs is always improving, but is known
to be idiosyncratic.

You don't need to install the following tools because they are provided through
Nix:

- [Glasgow Haskell Compiler (GHC)][ghc]
- [Cabal][cabal]
- [Ghcid][ghcid].

However, external to this project, you may optionally use (discussed in this
writeup):

- [Docker][docker]
- [the Emacs text editor][emacs]
- [Dante (for Emacs and Haskell)][dante]
- [Haskell Stack][stack].


## Features

Here's some language-agnostic features of this project:

- supports mixed platform build (C, Haskell, almost anything else) via Nix
- managed concisely in a ["call-package"][callpackage]-style in
  [one file](./build.nix)
    - pins [nixpkgs][nixpkgs] to a specified version for deterministic builds
    - shows how to override individual packages in the pinned set
- organized into modules (Nix expressions get twisted easily)
- shows how to integrate into Docker
- can generate a license report for many runtime dependencies
- supports typical Nix `nix-build`, `nix-env`, and `nix-shell` calls
- supports ignoring files
    - avoids needless rebuilds
    - keeps ignored files out of `/nix/store`.

Here's some features specific to Haskell:

- automatically calls [cabal2nix][cabal2nix] for us (Haskell projects are
  Nix-free)
- supports statically linked binaries with a compact dependency set
- illustrates workaround to keep dependencies compact even if they use Cabal
  "data-files" (using [`replace-literal`](#replace-literal))
- provides recursive [ctags/etags][ctags] generation (using
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
fetching path ‘/nix/store/48f5qyygrikrvja43i493sjmskzcpfjj-mirrors-list’...
fetching path ‘/nix/store/xlap4dwqfgjm581j07ww0ygavfmzixqi-curl-7.52.1-man’...
fetching path ‘/nix/store/nqg4ngyawb9w02r06f4n6nbqwfbrd7cg-gawk-4.1.3’...
fetching path ‘/nix/store/mypmpnb3m5bcg1q70gynpz05sr8czi16-gcc-5.4.0-man’...
...
shrinking RPATHs of ELF executables and libraries in /nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0
shrinking /nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0/bin/example-app
stripping (with flags -S) in /nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0/bin
patching script interpreter paths in /nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0
/nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0
```

The `nix-build` invocation without any arguments uses the Nix expression in
[./default.nix](./default.nix).  This resultant build is stored in
`/nix/store`, but `nix-build` leaves behind "result" symlink in the current
directory for our convenience:

```
result -> /nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0
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
example-nix$ nix-store --query --requisites result | xargs du -sh | sort -n
900K    /nix/store/qmv9i11bwcqvj8cll14amnia2b2a97dj-zlib-bindings-0.1.1.5
816K    /nix/store/xj9q1wifby39xcz2805s55k8gharf44z-io-streams-haproxy-1.0.0.1
764K    /nix/store/bhxacgb69myscr3yh4v4q0mszcj26wik-case-insensitive-1.2.0.7
...
1.1M    /nix/store/3ha5x9502a93j0n3a015bz0xz7rjnym6-ekg-0.4.0.12
1.1G    /nix/store/mjcifpjgihsqibwsmlimz40h08y1lv9v-ghc-8.0.1
```

As you can see, `example-app` as compiled pulls dependencies that require a lot
of space.  This can be a serious impedance if we want to package/distribute a
runnable version of our application.

It turns out, even when statically linking, `ekg` has a quirk that pulls in
lots of dependencies (which is why it was intentionally chosen for our example
application).  Fortunately, this project
illustrates [a technique](#replace-literal) to address this problem.

If you look at [./default.nix](./default.nix), you'll see that it references
the "example-app" attribute in a set defined
in [./build.nix](./build.nix#L61-L71).

We could have built this explicitly with `nix-build`:

```
example-nix$ nix-build --attr example-app ./build.nix
/nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0
```

Notice it didn't build anything, because it used the cached result from
earlier.

The "example-bundle" attribute references a dependency-compacted version of our
application, which we can build and run similarly:

```
example-nix$ nix-build --attr example-bundle ./build.nix
these derivations will be built:
  /nix/store/6x3wj4imxl5vzymyks4arp3259f1fhl9-ekg-assets.drv
  /nix/store/gw55rz5fj4q1x4c8khf5jn8dcxjss2qc-example-bundle.drv
these paths will be fetched (0.02 MiB download, 0.04 MiB unpacked):
  /nix/store/lph984c2kz24ldp53cnffscj5f358hxk-replace-2.24
...
building path(s) ‘/nix/store/106cgv71403fsrpmvky3046i1rvw7lrb-example-bundle’
prepatched GBN: /nix/store/sadnnm26mfddpdip7z75a33fg7k202xp-example-app-0.1.0.0/bin/example-app
   patched GBN: /nix/store/106cgv71403fsrpmvky3046i1rvw7lrb-example-bundle/bin/example-app
Scanning binary file /nix/store/106cgv71403fsrpmvky3046i1rvw7lrb-example-bundle/bin/example-app (2748880 bytes)...
"/nix/store/3ha5x9502a93j0n3a015bz0xz7rjnym6-ekg-0.4.0.12" ->  "/nix/store/crn26c4canjpjbm6cxq2kb1z4i1q8y60-ekg-assets\0" (offset: 2324272)
The binary file /nix/store/106cgv71403fsrpmvky3046i1rvw7lrb-example-bundle/bin/example-app would have had one string replaced
/nix/store/106cgv71403fsrpmvky3046i1rvw7lrb-example-bundle
```

This replaced our "result" symlink with another one.  This time, when we look
at the transitive closure of our dependencies, we see a much smaller and
compact set:

```
example-nix$ nix-store --query --requisites result | xargs du -sh
23M	/nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24
4.7M	/nix/store/jar52969wyf10sh2wj62ipfjiw7xaq2j-gcc-5.4.0-lib
684K	/nix/store/a9wlwd8cfzyprwwkib5ibi3cn56v165y-gmp-6.1.1
220K	/nix/store/crn26c4canjpjbm6cxq2kb1z4i1q8y60-ekg-assets
124K	/nix/store/gdw5qz99ihbv3l3nc91jdynfm3va46qq-zlib-1.2.10
3.4M	/nix/store/nxcny3iwkq6bn0y16l2m6jbrblwnwmrl-openssl-1.0.2j
2.7M	/nix/store/106cgv71403fsrpmvky3046i1rvw7lrb-example-bundle
```

### Putting the app in a Docker container

The "example-tarball" attribute of the Nix expression
in [./build.nix](./build.nix) has an expression that takes the transitive
closure of `example-bundle` and puts it into a tarball:

```
example-nix$ nix-build --attr example-tarball ./build.nix
these derivations will be built:
  /nix/store/4wa2mwgzlbxqw3h6f7qbc5jvkirw2a2p-runtime-deps.drv
  /nix/store/nik5hjask7dji8h7vqx9ngb7q16gykyd-example-bundle.tar.gz.drv
these paths will be fetched (0.01 MiB download, 0.02 MiB unpacked):
  /nix/store/8b8cf0blrjnchnckfh6alfi795pfrw6n-stdenv
fetching path ‘/nix/store/8b8cf0blrjnchnckfh6alfi795pfrw6n-stdenv’...
...
/nix/store/9g4h69wama4agd9wgqx4s7sam382nx78-example-bundle.tar.gz
```

Again, the "result" symlink has changed, and now it points to the tarball we've
built.

We can use Docker to unpack this tarball into a "scratch" Docker image, and
then run it.  If you're running Linux, this works fine.  But if you built the
example application on a Mac, the built binary will be incompatible with
Docker.

In [./bin](./bin) there's two scripts:

- [build\_native-package\_docker](./bin/build_native-package_docker) (for
  Linux)
- [build\_docker-package\_docker](./bin/build_docker-package_docker) (for
  Macs and Linux)

If you have Docker installed, these scripts can be run from a fresh checkout,
and illustrate everything from building `example-tarball` to putting it in a
Docker image, and finally running it:

```
example-nix$ ./bin/build-docker_package-docker
/nix/store/9g4h69wama4agd9wgqx4s7sam382nx78-example-bundle.tar.gz
...
Sending build context to Docker daemon 20.56 MB
Step 1/4 : FROM scratch
 --->
Step 2/4 : ADD example-app.tar.gz /
 ---> 318bc5812246
Removing intermediate container 50fbb7ca93fa
Step 3/4 : EXPOSE 8081
 ---> Running in 17c665e292c1
 ---> 37b28d0bd444
Removing intermediate container 17c665e292c1
Step 4/4 : ENTRYPOINT /bin/example-app
 ---> Running in ff24c938190b
 ---> 4c9100d48426
Removing intermediate container ff24c938190b
Successfully built 4c9100d48426
EKG running on http://localhost:8081
hit any key to quit
```

What the Mac-friendly script does differently is run `nix-build` from within a
Docker container, so the artifact is built for Linux instead of a Mac.

Notice the Docker image produced is remarkably small (under 35MB):

```
example-nix$ docker images nix-example-app
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
nix-example-app     latest              a40ab024f5c1        16 seconds ago      32.5 MB
```


### Making a runtime license report

Figuring out whether an application is properly licensed requires going through
all the licenses of all the dependencies used.

Unfortunately, Nix doesn't offer a complete solution for this, but can help a
little.

The "example-extra.licenses" attribute of the Nix expression
in [./build.nix](./build.nix) has an expression that generates a JSON file with
license information for many of the dependencies required in the /nix/store.

There are two important omissions in this report:

- no mention of statically compiled libraries
- some dependencies have missing license information

When a library is statically compiled, Nix loses track of the dependency.  To
get a more accurate license report, create it from a dynamically-generated
variant instead.  The report generated by this project includes results for a
dynamically linked version of `example-app` and `example-bundle` (statically
linked and compacted) so you can see the difference.

Additionally, you may notice a project listed as a dependency with no license
information.  This is
a [limitation of current state of the art in Nix](#incomplete-license-report).

The [bin/licenses](./bin/licenses) script builds this JSON license report, and
as a convenience runs it through the `jq` tool for formatting, and then through
`less` for paging:

```
example-nix$ bin/licenses
{
  "example-app": [
    {
      "homepage": "https://github.com/basvandijk/monad-control",
      "license": {
        "fullName": "BSD 3-clause \"New\" or \"Revised\" License",
        "shortName": "bsd3",
        "spdxId": "BSD-3-Clause",
        "url": "http://spdx.org/licenses/BSD-3-Clause"
      },
      "path": "/nix/store/1kjpiqgbs7sgj4ay0cvqylvjk3vid5ad-monad-control-1.0.1.0"
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
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/ghc

[nix-shell:example-nix]$ which ghci
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/ghci

[nix-shell:example-nix]$ which ghcid
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/ghcid

[nix-shell:example-nix]$ which cabal
/nix/store/nkx20y8vfjragx4iljv36linnkjyvbj5-ghc-8.0.1-with-packages/bin/cabal

[nix-shell:example-nix]$ which nix-tags-haskell
/nix/store/46zfy10dy0jhqyj8lf51faiz78w43vgh-nix-tags-haskell/bin/nix-tags-haskell

[nix-shell:example-nix]$ which cabal-new-watch
/nix/store/nydb2j7bybff4445ifphnbl2j40jz1iy-cabal-new-watch/bin/cabal-new-watch

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

Note, these "new-\*" commands have been released as such for testing.  Once
they've been deemed stable, the normal "configure/build/repl" will be replaced
with their "new-" counterparts, which will go away.

In an effort to help to contribute towards these commands' success, this
project does not shy from using them, and encourages people to try them out.


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

Note, the reason Ghcid is faster than a normal build with Cabal or Stack is
because, it's using a REPL session, which it uses to reload modules.  This
provides a faster compilation, but sometimes error messages get out of sync,
and you have to restart Ghcid.


#### Editor tags files

If you use a text editor like Emacs of Vim, you can navigate multiple projects
fluidly using [ctags/etags][ctags].  For Haskell, this project's Nix shell
environment provides a `nix-tags-haskell` script to create a tags file:

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

Note, Dante like Ghcid uses a REPL for faster compilation than a normal Cabal
or Stack build.  So like Ghcid, its errors can fall out of sync with a true
build, and you'll need to restart the session with `M-x dante-restart`.


#### Haskell Stack

Finally, if you have Stack installed, you can run it from the root project:

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

Use Dante if you want to avoid this problem with Stack entirely.


#### Stackless change-triggered builds

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

The best way to explore this project further is to jump into the source code.
Here's a roadmap:

| File/Directory               | Description                                  |
| -----------------------------| -------------------------------------------- |
| bin/                         | scripts to illustrate this project           |
| build.nix                    | root-level project configuration             |
| cabal.project                | `cabal` "new-\*" multi-project configuration |
| default.nix                  | `nix-build` configuration                    |
| modules/app                  | Haskell "example-app" application using "example-lib" library |
| modules/bundle               | Minimization of "example-app"                |
| modules/ekg-assets           | Assets stripped from `ekg` Nix derivation    |
| modules/lib                  | Haskell "example-lib" library                |
| modules/pkgs-make/           | the Nix expression driving this project      |
| modules/stack                | Nix expressions for Stack integration        |
| shell.nix                    | `nix-shell` configuration                    |
| stack.yaml                   | `stack` configuration                        |

Most of the supporting Nix code is
in [./modules/pkgs-make/](./modules/pkgs-make), which is organized as follow:

| File/Directory               | Description                                  |
| -----------------------------| -------------------------------------------- |
| default.nix                  | entry point for pkg-make Nix expression      |
| haskell.nix                  | Haskell-specific Nix expressions             |
| extn/                        | more derivations to extend `nixpkgs`         |
| lib/                         | more Nix expressions to extend `nixpkgs`     |
| overrides/                   | default overrides for `nixpkgs` derivations  |


## Hacks

There are a few hacks in this project that would be wonderful to improve, but
are probably fine for most contexts:

- license report may often be incomplete
- automation of cabal2nix is fragile
- `replace-literal` /nix/store reference replacement (for minimization)

The following discussion assumes some deeper knowledge of Nix and `nixpkgs`.


### Incomplete license report

We find runtime dependencies by looking through the generated artifact in
`/nix/store`, and finding all further references within `/nix/store`.  This
doesn't tell us anything about compile-time dependencies.  Also, `/nix/store`
doesn't contain license information.

To match the detected dependencies with license information, we do a heuristic
crawl through `nixpkgs` tree, starting with the derivation for our built
artifact.  Sometimes we don't find what we want.

Also, this license report is currently limited to runtime dependencies (which
is the common case for most inquiries).  Compile-time dependencies explodes to
a much larger set, and offers more challenges due to how some Nix expressions
use string-interpolation.

Finally, the accuracy of the report is only as good as the information in
`nixpkgs`.  For instance, `gmp` is currently listed as GPL-licensed, when it's
actually dual-licensed with both GPL and LGPL.

Hopefully this report is still useful, provided you understand the caveats.


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


[cabal2nix]: https://github.com/NixOS/cabal2nix
[cabal2nix-issue]: https://github.com/NixOS/nix/issues/1148
[cabal]: https://haskell.org/cabal
[cabal-new]: http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/
[cabal-new-issue]: https://github.com/haskell/cabal/issues/3741
[callpackage]: http://lethalman.blogspot.com/2014/09/nix-pill-13-callpackage-design-pattern.html
[ctags]: http://ctags.io
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
[projectile]: http://projectile.readthedocs.io
[replace-call]: ./modules/bundle/builder.sh#L9-L10
[stack]: http://haskellstack.org
[stack-nix]: https://docs.haskellstack.org/en/stable/nix_integration/
