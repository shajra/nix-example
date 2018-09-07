- [Introduction](#org13ddb8d)
- [Motivation For Nix](#org9b58d29)
- [Exploring a fresh Nix installation](#orge9663a9)
- [The Nix language](#org27bdbef)
  - [Numeric literals](#org108ba79)
  - [Strings](#org640c983)
  - [Let-expressions](#org0deb4bf)
  - [String interpolation](#org34e3817)
  - [Functions](#org45ea395)
  - [Lists](#orgf09e172)
  - [Attribute sets](#org08547e9)
  - [Paths](#orga72cac1)
  - [Importing](#orgf2cffa2)
- [Nixpkgs and building](#org21c3eb9)
- [Calling `nix`](#org0ecb600)
- [Finding packages](#org47f409a)
- [Running](#orgc74af1f)
- [Installing](#org7b899a3)
- [Uninstalling](#org8e7c90e)
- [Inspecting dependencies](#org6286086)
- [Cleaning up](#org5d305a5)
- [Developing with `nix-shell`](#orgc6dde6f)



<a id="org13ddb8d"></a>

# Introduction

This tutorial illustrates some basic operation of Nix. We'll introduce some some concepts, terminology, and command-line utilities. That will provide some setup for building our own projects with Pkgs-make in later tutorials.


<a id="org9b58d29"></a>

# Motivation For Nix

Nix at its heart is a package manager that can build from source arbitrary packages written in a variety of languages, and also manage concurrent per-user installations.

Every programming language ecosystem has its particular set of tools for building and managing libraries (for instance, Python has PIP/Conda and Ruby has Gems). Reaching for a tool like the Nix package manager may at first seem redundant. For us, two central benefits of Nix are

-   ease to integrate artifacts from a variety of languages ecosystems
-   extreme build/deployment reproducibility.

Many languages use FFI wrapper libraries over native C libraries. Or they require oddly-compiled external dependencies for compilation or runtime. Furthermore, some domains like machine learning require the integration of different applications developed in languages like C/C++, R, or Python.

To maintain reproducibility while integrating such heterogeneous builds, we could use something like [Docker](https://www.docker.com). But most of these solutions involve opaque artifacts that are hard to trust. For instance, when we use a “debian:8.7” image from Docker Hub, it's not clear how to reproduce it if we have to. We just trust the community to have created a reasonable image which is then frozen with a version identifier “8.7” and published on Docker Hub. Furthermore, Docker makes no guarantee that two images created from the same Dockerfile will be equivalent.

The Nix package manager provides a better architecture for reproducible builds by treating the build process as a mathematical function. In Nix, these functions are written in a language also called Nix. And as in math, Nix expressions yield the same result, even when called at different times or on different machines (just as we trust `1 + 1` to evaluate to `2` on any machine at any time).

This allows us to reference and cache results with far more precision and resolution than we can with Docker's Dockerfiles. The Nix ecosystem even goes further by patching compilers to make compiled artifacts bit-for-bit reproducible. And because Nix is based on mathematical functions, there's lots of composition (as in `f°g`), which you can use to mix your C, R, Haskell, and so forth.

Central to Nix is a special Git repository hosted on GitHub called [Nix Packages (Nixpkgs)](https://github.com/NixOS/nixpkgs), which is a large tree of Nix expressions for all kinds of tools and libraries built from a variety of source languages/platforms (enough to support [an entire operating system called NixOS](https://nixos.org/nixos)).

So once we learn the language of Nix, we can build almost anything we want, using Nixpkgs as a starting point. Nix can be very hackable without abandoning a principled architecture.

Nix is not without its problems, as [Gabriel Gonzalez points out](https://github.com/Gabriel439/haskell-nix#background). Nix could use more documentation and tools to ease adoption. Also it takes a lot of work to curate all of Nixpkgs, so you may occasionally find yourself writing your own Nix expressions, or contributing back to the Nix ecosystem. Fortunately, Nix expressions for a particular library or package can be compact.

For many of us the benefits of Nix outweigh the inconveniences. Hopefully, projects and tutorials like this can help tip the balance further.


<a id="orge9663a9"></a>

# Exploring a fresh Nix installation

By design, Nix sequesters almost all of its installation under `/nix`. This way, Nix has extremely few dependencies on the surrounding environment.

For non-NixOS installations of Nix, if you deleted `/nix`, you would be very close to a clean uninstallation. Beyond files in `/nix` an installation may also include:

-   a few symlinks in users' home directory

-   a “nix-daemon” service installed into systemd (Linux) or launchd (Mac).

And naturally, and if you run any programs, they might also leave behind application-specific data.

There are two directories under `/nix`:

-   `/nix/store`: where programs are installed

-   `/nix/var`: where mutable metadata, symlinks, caching, and indexing is managed.

Typically, you'll never manage files under `/nix` directly. Instead you'll use the Nix command-line tools. To get packages into `/nix/store` we first need a Nix expression. These expressions can either come from a third party like the central Nixpkgs GitHub repository. Or we can write our own.


<a id="org27bdbef"></a>

# The Nix language

To better discuss the Nix command-line tools and also Nixpkgs, we'll introduce the Nix language a little. This is no substitute for the [official Nix language documentation](https://nixos.org/nix/manual/#ch-expression-language), which is surprisingly not that long for a programming language; Nix does not have much syntax relative to other general-purpose programming languages.


<a id="org108ba79"></a>

## Numeric literals

We can play around with the Nix language with the `nix eval` command. As with many languages, we can use Nix as a simple calculator by passing Nix expressions to it:

```shell
nix eval '(1 + 1)'
```

    2

Note, the parentheses are necessary with `nix eval` when passing expressions directly on the command-line. The single-quotes are to pass the whole parenthesized expression as one argument in a shell invocation. If you want less boilerplate, you can also use the `nix repl` command for an interactive session.

Nix supports a variety of types you'd expect for a programming language, and we get some literal syntax for typical primitives:

```shell
nix eval '(builtins.typeOf 1)'
nix eval '(builtins.typeOf 1.0)'
```

    "int"
    "float"


<a id="org640c983"></a>

## Strings

As you may expect from other languages, Nix supports string literals with the conventional double quote syntax:

```shell
nix eval '(builtins.typeOf "hello")'
```

    "string"

Nix also supports multi-line strings with two consecutive single quotes:

```shell
nix eval "(''
    line 1
      line 2
   line 3
'')"
```

    " line 1\n   line 2\nline 3\n"

The left-most token in any line establishes a left margin. In the example above, this is “line 3”. Beyond these strings, Nix does not have syntactically significant whitespace.

We concatenate strings with the `+` operator:

```shell
nix eval '("a" + "b")'
```

    "ab"


<a id="org0deb4bf"></a>

## Let-expressions

For the most part, Nix is a lazily evaluated purely functional programming language. We don't imperatively run commands that change things, and in general, the language restricts us from doing so. There's not even syntax to do so. At the top-level, you can't bind values to variables as you may in other imperative languages. Instead, we are only ever working with a single Nix expression at a time, and we bind values to names locally with *let-expressions*:

```shell
nix eval '(let a = 1; b = 2; in a + b)'
```

    3

Note that semicolons are mandatory in all Nix forms that have them, including let-expressions. Because of Nix's strict parsing you can neither elide semicolons, nor put extra ones.


<a id="org34e3817"></a>

## String interpolation

Sometimes we build up small code snippets inline in a Nix expression, so it's useful to have string interpolation support. This is done with the following syntax:

```shell
nix eval '(
    let foo = "Foo";
	bar = "Bar";
    in "${foo + bar} is a terrible name")'
```

    "FooBar is a terrible name"

String interpolation is supported by both normal and multi-line strings.

Note that unlike shell scripts, the curly braces are not optional for string interpolation in Nix. This works out in our favor if we're writing shell scripts inline in a Nix expression, because we can use `$name` for shell string interpolation and `${nix_expr}` for Nix string interpolation. If this is not enough, though not covered in this tutorial, there is a syntax for suppressing interpolation in both normal and multi-line Nix string literals.


<a id="org45ea395"></a>

## Functions

Nix has first class functions. Functions take in only one argument at a time, and use a colon to separate the parameter name from the body of the function. Furthermore, Nix uses whitespace for function application:

```shell
nix eval '(builtins.typeOf (a: a + 1))'
nix eval '((a: a + 1) 2)'
```

    "lambda"
    3

Since functions take only one argument at a time, they are naturally curried, and we encode functions taking multiple arguments with functions returning functions:

```shell
nix eval '((a: b: a + b) 1 2)'
```

    3


<a id="orgf09e172"></a>

## Lists

Nix also has list literals which use square brackets and are whitespace-delimited:

```shell
nix eval '(builtins.typeOf [1 2 3 4 5])'
```

    "list"

We can append lists together with the `++` operator:

```shell
nix eval '([1 2] ++ [3 4])'
```

    [ 1 2 3 4 ]


<a id="org08547e9"></a>

## Attribute sets

Very importantly, Nix has a kind of map called an *attribute set* that is specialized to have textual indices called *attributes* that index values of arbitrary types. It uses the following syntax:

```shell
nix eval '(builtins.typeOf { foo = 1; bar = 2; })'
nix eval '({ foo = 1; bar = 2; }.bar)'
```

    "set"
    2

Note, `builtins` is just an attribute set that is in scope by default. And `typeOf` is just an attribute that maps to a function that returns a string indicating the type of the argument.

Often used in Nix expressions, we can overlay sets on top of each other with the `//` operator:

```shell
nix eval '({ foo = 1; bar = 2; } // { bar = 3; baz = 4; })'
```

    { bar = 3; baz = 4; foo = 1; }

Additionally, we can prefix set literals with the `rec` keyword to get recursive sets. Recursive sets allow values in a set to reference attributes by name:

```shell
nix eval '(rec { foo = bar; bar = 2; }.foo)'
```

    2

Without the `rec` keyword, we'd get an error:

```shell
nix eval '({ foo = bar; bar = 2; }.foo)' 2>&1 || true
```

    error: undefined variable 'bar' at (string):1:10

If a function accepts an attribute set as an argument, we can have Nix unpack the argument as a convenience with the following syntax:

```shell
nix eval '(({ foo, bar }: foo + bar ) { foo = 1; bar = 2; })'
```

    3

We can still bind the argument to a name if we want using a “@” form, and if we want to accept sets with other attributes we can use a “&#x2026;” form:

```shell
nix eval '((s@{ foo, ... }: foo + s.bar ) { foo = 1; bar = 2; baz = 3; })'
```

    3

Attribute sets also support an additional syntactic convenience when pulling in locally bound values as attributes, which comes up a lot in Nix:

```shell
nix eval '(let a = 3; in { a = a; })'
```

    { a = 3; }

Using the `inherit` key word, we accomplish the same thing while simplifying the `a = a` to:

```shell
nix eval '(let a = 3; in { inherit a; })'
```

    { a = 3; }


<a id="orga72cac1"></a>

## Paths

Because the Nix language is domain-specific for building packages, it also has a *path* type, which is indicated by an identifier with a slash:

```shell
nix eval '(builtins.typeOf some/path)'
nix eval '(some/path)'
```

    "path"
    /home/shajra/src/shajra/example-nix/tutorials/0-nix-intro/some/path

As part of a standard Nix installation, there's a special environment variable `NIX_PATH` that makes paths dynamically available to Nix expressions:

```shell
env | grep NIX_PATH
```

    NIX_PATH=nixpkgs=/home/shajra/.nix-defexpr/channels/nixpkgs

The setting of `NIX_PATH` shown above maps the user local path `~/.nix-defexpr/channels/nixpkgs` to the identifier `nixpkgs`, which can be accessed in a Nix expression using an angle bracket syntax:

```shell
nix eval '(<nixpkgs>)'
```

    /home/shajra/.nix-defexpr/channels/nixpkgs

Remember that a major motivation for using Nix is for deterministic builds. We have to be extremely careful when using angle brackets to access `NIX_PATH` because this can make the same Nix expression evaluate differently on different systems.

If you inspect `~/.nix-defexpr/channels/nixpkgs` you'll notice it's a symlink pointing to a snapshot of Nixpkgs in `/nix/store`. This symlink is set up during Nix installation, and can be upgraded with a call to `nix-channel --update`.


<a id="orgf2cffa2"></a>

## Importing

We can import paths. If the path is a file, it's loaded as a Nix expression. If it's a directory, a file called “default.nix” is loaded within it. A snapshot of Nixpkgs, for example, has a `default.nix` file at its root, so we can import the path directly:

```shell
nix eval '(builtins.typeOf (import <nixpkgs>))'
```

    "lambda"

We see here that Nixpkgs is a function. We'll talk more about the nature of the Nixpkgs function next.


<a id="org21c3eb9"></a>

# Nixpkgs and building

Nixpkgs is a function that takes an attribute set as configuration and returns a nested attribute set containing thousands of packages and functions. An empty attribute set passed to the Nixpkgs function configures it with defaults:

```shell
nix eval '(builtins.typeOf (import <nixpkgs> {}))'
```

    "set"

One of the packages in Nixpkgs is the [GNU Hello](https://www.gnu.org/software/hello/) program, mapped to the attribute `hello`:

```shell
nix eval '(builtins.typeOf (import <nixpkgs> {}).hello)'
```

    "set"

At this point, packages in Nixpkgs appear to just be attribute sets, but there's a special schema for attributes in a set that allow various Nix command-line utilities to recognize the set as a *derivation* for a package.

For instance, we can use the `nix show-derivation` to view some metadata about how Hello will be built:

```shell
nix show-derivation '((import <nixpkgs> {}).hello)'
```

    {
      "/nix/store/c271aryv5jzm0aq640lb7gw0102id5f5-hello-2.10.drv": {
        "outputs": {
          "out": {
            "path": "/nix/store/188avy0j39h7iiw3y7fazgh7wk43diz1-hello-2.10"
    …
          "system": "x86_64-linux",
          "version": "2.10"
        }
      }
    }

We've cropped the details of the derivation above, but you can get a sense that the derivation itself is also saved into `/nix/store` as a “\*.drv” file, and when the application is actually built, it is put in another output path, also in `/nix/store`.

Also important to know, when we interpolate a derivation into a string, we inject as a string the path of the derivation's output:

```shell
nix eval '("our value is ${(import <nixpkgs> {}).hello}")'
```

    "our value is /nix/store/188avy0j39h7iiw3y7fazgh7wk43diz1-hello-2.10"

Note, that it may not exist there yet. We can build it with a call to `nix build`:

```shell
nix build --verbose '((import <nixpkgs> {}).hello)' 2>&1
```

    copying path '/nix/store/188avy0j39h7iiw3y7fazgh7wk43diz1-hello-2.10' from 'https://cache.nixos.org'...

If we were to dig into the Nixpkgs repository, we'd find [code for building Hello from source](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/misc/hello/default.nix). But notice that we didn't end up fetching the Hello released tarball, unpacking it, and building it. Instead, we got a cached version of it from <https://cache.nixos.org>.

The Nix community builds and caches packages from Nixpkgs in a service called [Hydra](https://nixos.org/hydra/), which <https://cache.nixos.org> fronts. If Hydra has the build you're trying to build, it's pulled down and cached locally in `/nix/store`. Otherwise, everything is built from source and cached in `/nix/store`.

You've probably also noticed the hashes in names of `/nix/store`'s contents. Nix meticulously hashes the contents of all the inputs for any package's derivation. If a single bit of the input changes, then the resultant hash will be different. This is what gives us confidence that the artifacts built by Hydra are more or less the same as what we'd have built locally.

`nix build` leaves behind a “result” symlink in the current directory for our convenience so we don't have to figure out how to find our build in `/nix/store`:

    result -> /nix/store/188avy0j39h7iiw3y7fazgh7wk43diz1-hello-2.10
    ├── bin
    │   └── hello
    └── share
        ├── info
        ├── locale
        └── man
    
    5 directories, 1 file

`nix build` also supports a `--no-link` switch if we want to build without leaving behind this “result” symlink.


<a id="org0ecb600"></a>

# Calling `nix`

We've shown thus far that Nix expressions can be passed to the `nix` utility but must be surrounded by parentheses.

When we call `nix` with an argument not parenthesized, the argument is interpreted as an attribute of an attribute set, which is by default built from of `NIX_PATH`.

Consider the following setting of the `NIX_PATH` environment variable:

```shell
NIX_PATH=nixpkgs=/home/shajra/.nix-defexpr/channels/nixpkgs
```

With this setting of `NIX_PATH`, the set `nix` would choose an attribute from would be:

```nix
{
    nixpkgs = import /home/shajra/.nix-defexpr/channels/nixpkgs {};
}
```

Note that `nix` automatically applies the empty set to any function that's detected when imported.

So we instead of building GNU Hello with this invocation:

```shell
nix build '((import <nixpkgs> {}).hello)'
```

we could just invoke `nix` with:

```shell
nix build nixpkgs.hello
```

Additionally, if we don't want the attribute set built implicitly from `NIX_PATH`, we can use the `--file` switch for `nix` to specify explicitly a path to be imported and select attributes from.


<a id="org47f409a"></a>

# Finding packages

We took for granted above that Nixpkgs had a package on the `hello` attribute.

We can find programs in Nix with a `nix search` invocation:

```shell
nix search hello
```

    Attribute name: nixpkgs.gnome3.iagno
    Package name: iagno
    Version: 3.28.0
    Description: Computer version of the game Reversi, more popularly called Othello
    
    Attribute name: nixpkgs.javaPackages.mavenHello_1_0
    Package name: maven-hello
    Version: 1.0
    Description: Maven Hello World
    
    Attribute name: nixpkgs.javaPackages.mavenHello_1_1
    Package name: maven-hello
    Version: 1.1
    Description: Maven Hello World
    
    Attribute name: nixpkgs.hello
    Package name: hello
    Version: 2.10
    Description: A program that produces a familiar, friendly greeting
    
    Attribute name: nixpkgs.hello-unfree
    Package name: example-unfree-package
    Version: 1.0
    Description: An example package with unfree license (for testing)


<a id="orgc74af1f"></a>

# Running

If you've never run the GNU Hello program, it's not too interesting. It's just a small C program useful for tutorials like this. We can run it using the “result” symlink left behind by an invocation of `nix build`:

```shell
result/bin/hello
```

    Hello, world!

Or we can also run it directly without the symlink using `nix run`:

```shell
nix run nixpkgs.hello --command hello
```

    Hello, world!

This invocation makes an environment in which we have the `nixpkgs.hello` package on our path (we can put other packages as well), and then we run the command after the `--command` switch. See `nix run --help` for more information.


<a id="org7b899a3"></a>

# Installing

`~/.nix-profile` is a symlink that follows to a *profile* under `/nix/var/nix/profiles` managed by `nix-env`. The profile furthermore points to an aggregated symlink tree of various programs installed into the profile by `nix-env`, often called an *environment* (though a very overloaded term). Programs installed by `nix-env` are made available to us by putting `~/.nix-profile/bin` on our `PATH`.

For example, with our `PATH` set up the following invocation of `nix-env` installs the Hello application so we can call it directly, rather than calling it via a “result” symlink or a `nix run` invocation:

```shell
nix-env --install --attr nixpkgs.hello 2>&1
```

    installing 'hello-2.10'
    building '/nix/store/32anrkb2hrzd74fq7hwawaz7x4mp59g5-user-environment.drv'...
    created 36 symlinks in user environment

```shell
which hello
```

    /home/shajra/.nix-profile/bin/hello

We can query with `nix-env` to see what's in our profile:

```shell
nix-env --query | grep hello
```

    hello-2.10

Every time we install an application with `nix-env` a new environment symlink tree is created in `/nix/store`. For posterity, `nix-env` keeps references to old versions under `/nix/var/nix/profiles`. You can use switches like `--rollback` with `nix-env` to revert back to previous states. See `nix-env --help` for more.


<a id="org8e7c90e"></a>

# Uninstalling

If you no longer want an installed application in your profile, you can uninstall it with `nix-env` as well:

```shell
nix-env --uninstall hello 2>&1
```

    uninstalling 'hello-2.10'

```shell
which hello || true
```

    hello not found


<a id="org6286086"></a>

# Inspecting dependencies

To find dependencies of a built package, Nix literally scans all files in a package (text and even binary) looking for textual references to “/nix/store/…”.

This makes it easy for Nix to find exactly what's needed for any compiled artifact to run. These references are often automated by Nix's tooling and library support, and it is generally considered a defect if a required runtime dependency is missing a reference point back to `/nix/store`.

This is important, because it allows for different compilations to rely on different versions of dependencies without conflicts. Our application shouldn't break if we do something like upgrade to our host operating system.

Also, without this clear and reliable method to detect dependencies, garbage collection wouldn't know how to keep needed dependencies around.

If we're curious about all the dependencies, we can use `nix path-info` to list them out.

```shell
nix path-info --recursive --closure-size nixpkgs.hello \
    | sort --numeric-sort --key 2
```

    /nix/store/83lrbvbmxrgv7iz49mgd42yvhi473xp6-glibc-2.27     25010496
    /nix/store/188avy0j39h7iiw3y7fazgh7wk43diz1-hello-2.10     25208024

Hello doesn't rely on much, just the standard glibc library. In real-world programs the dependencies can add up.


<a id="org5d305a5"></a>

# Cleaning up

Every time you build a new version of your code, it's stored in `/nix/store`. There is a command called `nix-collect-garbage` that purges unneeded packages. What keeps a package from being reclaimed by `nix-collect-garbage` are symlinks under `/nix/var/nix/gcroots`. These come in a few flavors including:

-   indirect links to the “result\*” links invocations like `nix build` may leave on our system

-   links to the currently active `nix-env` environment, as well as all previous generations saved away for posterity under `/nix/var/nix/profiles`.

If you delete a “result\*” link and call `nix-collect-garbage`, though some garbage may be reclaimed, you may find that an old `nix-env` environment is keeping the program alive. We can illustrate this here:

First, we'll do two `nix-collect-garbage` calls to show that we've reclaimed as much space as we can:

```shell
nix-collect-garbage; nix-collect-garbage
```

    2 store paths deleted, 0.01 MiB freed
    0 store paths deleted, 0.00 MiB freed

Then we can try to delete our symlink, but another garbage collection doesn't doesn't reclaim anything more:

```shell
rm result
nix-collect-garbage 2>&1
```

    finding garbage collector roots...
    removing stale link from '/nix/var/nix/gcroots/auto/qn37jdmf8zj32mzkp66nii4b8vwpd85s' to '/home/shajra/src/shajra/example-nix/tutorials/0-nix-intro/result'
    deleting garbage...
    deleting '/nix/store/trash'
    deleting unused links...
    note: currently hard linking saves -0.00 MiB
    0 store paths deleted, 0.00 MiB freed

Now with the `--delete-generations` switch of `nix-env` we can delete old generations of our environment:

```shell
nix-env --delete-generations old 2>&1
```

    removing generation 130
    removing generation 131

And then then garbage collection works as you'd expect:

```shell
rm result
nix-collect-garbage 2>&1
```

    finding garbage collector roots...
    deleting garbage...
    deleting '/nix/store/chpaa1va5rpcvvzhjw65dhd1ymxg4pv2-user-environment'
    deleting '/nix/store/32anrkb2hrzd74fq7hwawaz7x4mp59g5-user-environment.drv'
    deleting '/nix/store/rpn44f5ih4i2wfpw6apmyi1dp5q2n52n-env-manifest.nix'
    deleting '/nix/store/188avy0j39h7iiw3y7fazgh7wk43diz1-hello-2.10'
    deleting '/nix/store/trash'
    deleting unused links...
    note: currently hard linking saves -0.00 MiB
    4 store paths deleted, 0.39 MiB freed


<a id="orgc6dde6f"></a>

# Developing with `nix-shell`

When Nix builds a derivation, it sets up a clean and controlled environment in which to do the build. Thus far, we've not inspected how `nix build` works. Nix comes with a `nix-shell` utility that allows us to get the environment variables that would be set up for a build. For instance, to get into the environment used to build Hello, we could use `nix-shell` as follows:

```shell
nix-shell --expr 'import <nixpkgs> {}' --attr hello
```

This drops us into a bash shell we can use interactively. We can also use nix-shell with a `--run` switch to run a single command non-interactively Additionally, we can use the `--pure` switch to throw away the previous environment variables rather than overlay changes on top of them

Here's a small example:

```shell
nix-shell --pure --expr 'import <nixpkgs> {}' --attr hello --run 'env'
```

    WINDRES=/nix/store/vp40my263xk32f07788srrgl59rycsna-binutils-2.30/bin/windres
    propagatedBuildInputs=
    stdenv=/nix/store/fjxk3k59n102sqs572kkspdjy303kv45-stdenv-linux
    __ETC_PROFILE_SOURCED=1
    DISPLAY=:0
    …
    SIZE=/nix/store/vp40my263xk32f07788srrgl59rycsna-binutils-2.30/bin/size
    nativeBuildInputs=
    LD=/nix/store/9z650yzczx5zyhcw5n36ya6x4jq436mr-gcc-wrapper-7.3.0/bin/ld
    depsTargetTargetPropagated=
    _=/nix/store/rhzcwaz7xk6dq9z2h9yca0z7wx0nwg7c-coreutils-8.29/bin/env

We're using `--expr` to pass expressions directly to the command line. Without this switch, `nix-env` looks for file called `shell.nix` to import in the current working directory. If that's not found, it looks for `default.nix`. Otherwise, a file can be passed as a positional parameter.

`nix-shell` is great for the following two scenarios, which we'll explore in later tutorials:

-   debugging a Nix build/derivation

-   entering into a development environment for a project.

In our derivation we can include as build dependencies not only things like compilers that we absolutely need for a build, but also optional tools for developer conveniences. This means that with `nix-shell`, we have different tools installed per-project, rather than worrying about having the right tool installed at a system- or user-level.
