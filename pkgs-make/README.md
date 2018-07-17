- [Introduction](#orge5d5bb6)
- [Prerequisites](#org2a9b1d5)
- [Importing a Variant](#orgfb1008a)
- [Specification of functions and types](#org0e7bf37)
  - [Notation and `PkgsMake`](#org447c3dc)
  - [`PkgsMakeArgs`](#org1fa0268)
  - [`(PkgsMakeLib -> Set<Drv>)` argument](#orgf8cb9c0)
  - [Returned `Build`](#org52fc45f)
  - [Nix-shell environments](#org7fa679b)
  - [Haskell-specific configuration](#org318bd3b)
  - [Python-specific configuration](#org4adf3d0)
- [Navigating the code](#org0455fa7)



<a id="orge5d5bb6"></a>

# Introduction

Pkgs-make is a library that factors away boilerplate we often end up when writing Nix expressions based solely on [Nixpkgs](https://github.com/NixOS/nixpkgs).

If you have a C, Haskell, or Python project, this library should be usable as is. If you're using another language, it can likely be extended.

One of the accompanying tutorials provides [an overview of using Pkgs-make](../tutorials/1-pkgs-make/README.md) as well as [Haskell tutorial](../tutorials/2-haskell/README.md) and [Python tutorial](../tutorials/2-python/README.md) both showcasing usage of Pkgs-make.

This document specifies Pkgs-make in more rigor and completeness.


<a id="org2a9b1d5"></a>

# Prerequisites

At a minimum, you need to [install Nix](https://nixos.org/nix/manual/#chap-installation). Additionally, you may want to [install some development tools](../tools/README.md) to showcase what's possible with Pkgs-make.

Ideally, we manage addition per-project tools with Nix expressions, alleviating the need to manually install too many tools beyond Nix.


<a id="orgfb1008a"></a>

# Importing a Variant

Pkgs-make is just a Nix library, similar to Nixpkgs, so once we fetch the repository we just need to import it from a Nix expression. Please see the tutorials if you don't know how to do that.

There's a few paths we can import in this repository:

| Path                          | Variant |
|----------------------------- |------- |
| `example-nix`                 | curated |
| `example-nix/pkg-make`        | curated |
| `example-nix/variant/curated` | curated |
| `example-nix/variant/plain`   | plain   |

Sometimes changes haven't gotten into Nixpkgs. As an experiment, the Pkgs-make contributors curate a set of overrides for Nixpkgs. In particular, many of these overrides help keep some machine learning libraries more up-to-date.

As a result Pkgs-make has two variants:

-   plain (no overrides)
-   curated (with overrides)

By default, you get the curated variant. But you can import `variant/plain` if you don't prefer these overrides:

```text
…
pkgsMake = import "${pkgsMakePath}/variant/plain";
…
```

Please note, we don't have a lot of people managing this curation. Also, it would be even better if the work from curation within Pkgs-make could work back into Nixpkgs. Any help is much appreciated.


<a id="org0e7bf37"></a>

# Specification of functions and types


<a id="org447c3dc"></a>

## Notation and `PkgsMake`

Since Nix doesn't have an official type notation, for this discussion we'll assume the following:

| Type       | Description                                           |
|---------- |----------------------------------------------------- |
| `Nixpkgs`  | an imported nixpkgs (e.g. from `import <nixpkgs> {}`) |
| `Drv`      | a Nix derivation                                      |
| `Bool`     | a boolean                                             |
| `String`   | a string                                              |
| `Path`     | a path                                                |
| `[e]`      | a list with elements of type `e`                      |
| `Set<v>`   | an attribute set with values of type `v`              |
| `Function` | a function with arbitrary input and output types      |
| `i -> o`   | a function inputting type `i`, returning type `o`     |

Pkgs-make is a higher-order function with the following type:

```
PkgsMake = PkgsMakeArgs -> (PkgsMakeLib -> Set<Drv>) -> FinalBuild
```

where `PkgsMakeArgs`, `PkgsMakeLib`, and `Build` are all attribute sets (some nested). Here's some details of each structure:


<a id="org1fa0268"></a>

## `PkgsMakeArgs`

| Attribute        | Type          | Description                              |
|---------------- |------------- |---------------------------------------- |
| `nixpkgsRev`     | `String`      | commit ID of base `Nixpkgs`              |
| `nixpkgsSha256`  | `String`      | hash parity check for `rev`              |
| `bootPkgs`       | `Nixpkgs`     | `Nixpkgs` to use to fetch base `Nixpkgs` |
| `basePkgsPath`   | `Path`        | path of explicitly set base `Nixpkgs`    |
| `nixpkgsArgs`    | `NixpkgsArgs` | standard arguments for `Nixpkgs`         |
| `srcFilter`      | `SrcFilter`   | source filter replacing defaults         |
| `extraSrcFilter` | `SrcFilter`   | source filter extending defaults         |
| `overlay`        | `Overlay`     | overlay for `Nixpkgs` replacing defaults |
| `extraOverlay`   | `Overlay`     | overlay for `Nixpkgs` extending defaults |
| `haskellArgs`    | `HaskellArgs` | Haskell-specific arguments               |
| `pythonArgs`     | `PythonArgs`  | Python-specific arguments                |

Most of these arguments involve the retrieval and configuration of a base `Nixpkgs`. By default the dynamic `<nixpkgs>` is used as the boot `Nixpkgs` only used to fetch the real `Nixpkgs` used as a base (pinned to `rev`). If `basePkgsPath` is supplied explicitly, this fetch isn't performed at all, and `nixpkgsRev`, `nixpkgsSha256`, and `bootPkgs` are ignored. This allows you to configure `Nixpkgs` explicitly if you prefer. Otherwise, you can use the conveniences of this library.

`SrcFilter` is a similar to the standard Nix source filter predicate but with a extra leading `Set` parameter:

```
SrcFilter = Set -> Path -> Type -> Bool
```

These types for a `SrcFilter` have the following values:

| Parameter | Type   | Value                                             |
|--------- |------ |------------------------------------------------- |
| 1         | `Set`  | `lib` attribute of `NixPkgsLib` described below   |
| 2         | `Path` | `String` of the path to the file to be filtered   |
| 3         | `Type` | result of calling `builtins.typeOf` on the path   |
| result    | `Bool` | `false` to discard file/directory, `true` to keep |

`Overlay` is the standard overlay for `Nixpkgs`:

```
Overlay = NixpkgsSelf -> NixpkgsSuper -> Set<Drv>=
```

These types for an `Overlay` have the following values:

| Parameter | Type           | Value                                      |
|--------- |-------------- |------------------------------------------ |
| 1         | `NixpkgsSelf`  | final `Nixpkgs` after all overlays applied |
| 2         | `NixpkgsSuper` | `Nixpkgs` before overlay are applied       |
| return    | `Set<Drv>`     | overlaid packages.                         |

The language-specific `HaskellArgs` and `PythonArgs` are described more later, but their common attributes include the following:

| Attribute        | Type               | Description                        |
|---------------- |------------------ |---------------------------------- |
| `overrides`      | `Override`         | overrides replacing defaults       |
| `extraOverrides` | `Override`         | overrides extending defaults       |
| `srcFilter`      | `SrcFilter`        | source filter replacing defaults   |
| `extraSrcFilter` | `SrcFilter`        | source filter extending defaults   |
| `envMoreTools`   | `Nixpkgs -> [Drv]` | extra env tools replacing defaults |

`Override` is has the following type:

```
Override = Nixpkgs -> PkgsSelf -> PkgsSuper -> Set<Drv>
```

These types for an `Override` have the following values:

| Parameter | Type        | Value                                      |
|--------- |----------- |------------------------------------------ |
| 1         | `Nixpkgs`   | pinned `Nixpkgs` with all overlays applied |
| 2         | `PkgsSelf`  | packages before overriding                 |
| 3         | `PkgsSuper` | packages after overriding                  |
| return    | `Set<Drv>`  | overriding packages                        |

Note that `PkgsSelf` and `PkgsSuper` are platform-specific packages (Haskell or Python), not top-level Nixpkgs packages.


<a id="orgf8cb9c0"></a>

## `(PkgsMakeLib -> Set<Drv>)` argument

Pkgs-make will pass to you `PkgsMakeLib`, which has the following attributes:

| Attribute          | Type            | Description                                                 |
|------------------ |--------------- |----------------------------------------------------------- |
| `call.package`     | `Path -> Drv`   | standard call-package                                       |
| `call.haskell.app` | `Path -> Drv`   | call-package for Haskell executables                        |
| `call.haskell.lib` | `Path -> Drv`   | call-package for Haskell libraries                          |
| `call.python`      | `Path -> Drv`   | call-package for Python packages                            |
| `lib.nix`          | `Set<Function>` | `nixpkgs.lib` with [some extras](./lib/default.nix)         |
| `lib.haskell`      | `Set<Function>` | `nixpkgs.haskell.lib` with [some extras](./lib/haskell.nix) |

Note that `call.haskell.app` statically links the resultant derivation. Otherwise, it is the same as `call.haskell.lib`.

You use these arguments to return a `Set<Drv>`. The `Path -> Drv` functions expect a path to a Nix expression, which when imported should have the same function expected by a respective “callPackage” function in Nixpkgs:

| Attribute          | Expects `Function` As Input Of Same Type As |
|------------------ |------------------------------------------- |
| `call.package`     | `nixpkgs.callPackage`                       |
| `call.haskell.app` | `nixpkgs.haskellPackages.callPackage`       |
| `call.haskell.lib` | `nixpkgs.haskellPackages.callPackage`       |
| `call.python`      | `nixpkgs.pythonPackages.callPackage`        |

The main benefit of using the provided “call.\*” functions is that you can reference your own packages from other packages without the typical Nixpkgs boilerplate of creating an overlay of overrides. Additionally, these functions have built in builtin defaults, like source filtering and selecting a target compiler/interpreter.

The Haskell API is slightly different in that if you don't have a `default.nix` in the path you supply, one will be automatically generated from the Cabal file found in the path.

Also, the main difference between `call.haskell.lib` and `call.haskell.app` is that the latter additionally calls `lib.haskell.justStaticExecutables` on the derivation, to streamline the built artifact to compact statically compiled executables.


<a id="org52fc45f"></a>

## Returned `Build`

What Pkgs-make returns you is largely the same `Set<Drv>` you define, but augmented with a few extra attributes:

| Attribute     | Type      | Description                         |
|------------- |--------- |----------------------------------- |
| `env.haskell` | `Drv`     | for `nix-shell` Haskell development |
| `env.python`  | `Drv`     | for `nix-shell` Python development  |
| `nixpkgs`     | `Nixpkgs` | final fully overlaid `Nixpkgs`      |

From this returned set, you can select out the packages you want by attribute for packaging and distribution.


<a id="org7fa679b"></a>

## Nix-shell environments

The “env.\*” attributes in the returned `Build` set have derivations for use with `nix-shell`. `env.haskell` unifies all the dependencies of Haskell packages in your build, excluding packages you're developing locally. Similarly, `env.python` does the same for Python packages. This allows us to get multi-package project support from Nix.

Also, both `env.haskell` and `env.python` derivations have an added attribute `withEnvTools` of type `Nixpkgs -> [Drv]` for including additional development tools, beyond what's pulled in by dependencies.

Note that the Haskell and Python environments already include some common development tools by default. You can look at the source code for the [relevant Haskell code](./haskell/default.nix) and [Python code](./python/default.nix) to see what's included.


<a id="org318bd3b"></a>

## Haskell-specific configuration

`HaskellArgs` includes one more attribute:

| Attribute    | Type     | Description                                  |
|------------ |-------- |-------------------------------------------- |
| `ghcVersion` | `String` | which compiler to use (of the form “ghc822”) |


<a id="org4adf3d0"></a>

## Python-specific configuration

`PythonArgs` includes two more attribute:

| Attribute     | Type      | Description                                     |
|------------- |--------- |----------------------------------------------- |
| `pyVersion`   | `String`  | which interpretter to use (of the form “36”)    |
| `envPersists` | `Boolean` | whether to keep “development-mode” files around |


<a id="org0455fa7"></a>

# Navigating the code

If you'd like to follow the implementation, this project is organized as follows:

| File/Directory                            | Description                            |
|----------------------------------------- |-------------------------------------- |
| [haskell/](./haskell)                     | Haskell-specific Nix expressions       |
| [haskell/overrides/](./haskell/overrides) | curated Haskell package overrides      |
| [haskell/tools/](./haskell/tools)         | Haskell-specific Nix development tools |
| [lib/](./lib)                             | platform-agnostic functions            |
| [python/](./python)                       | Python-specific Nix expressions        |
| [python/overrides/](./python/overrides)   | curated Python package overrides       |
| [top/overrides/](./top/overrides)         | curated top-level package overrides    |
| [config.nix](./config.nix)                | configuration defaults                 |
| [default.nix](./default.nix)              | top-level entry point for convenience  |
| [tools.nix](./tools.nix)                  | stand-alone tools                      |
