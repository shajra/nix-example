- [About This Project](#sec-1)
- [Navigating This Repository](#sec-2)
- [Prerequisites](#sec-3)
  - [Knowledge](#sec-3-1)
  - [Technology](#sec-3-2)
    - [Nix](#sec-3-2-1)
    - [Docker](#sec-3-2-2)
    - [Additional developers tools](#sec-3-2-3)
    - [Things you don't need to install](#sec-3-2-4)
- [Feedback Desired](#sec-4)


# About This Project<a id="sec-1"></a>

This project illustrates using [the Nix package manager](https://nixos.org/nix) for programming heterogeneous, multi-module projects. Notably, it provides [a Nix library called Pkgs-make](./pkgs-make/README.md) to save some the boilerplate we might have to write when using Nix for managing a software lifecycle. Pkgs-make currently directly supports [Haskell](https://haskell.org) and [Python](https://www.python.org/) development, but has the potential to support other language platforms.

Use this project to learn and explore what's possible. It's set up for pedagogy, but you should be able to use Pkgs-make in real projects (a group of us does). There are many ways to organize code, and the beauty of Nix is that it's very flexible. So use Pkgs-make if you can, but be aware that there's many ways to achieve the same goal, and your needs will determine what's best.

# Navigating This Repository<a id="sec-2"></a>

We've tried hard to keep the code clean and self-documenting. Here's a survey of the project's layout with some links, each with their own README files explaining more:

| File/Directory                                    | Description                                        |
|------------------------------------------------- |-------------------------------------------------- |
| [tutorials/0-nix-intro/](./tutorials/0-nix-intro) | tutorial of basic Nix usage                        |
| [tutorials/1-pkgs-make/](./tutorials/1-pkgs-make) | tutorial of basic Pkgs-make usage                  |
| [tutorials/2-haskell/](./tutorials/2-haskell)     | tutorial of Haskell example using Pkgs-make        |
| [tutorials/2-python/](./tutorials/2-python)       | tutorial of Python example using Pkgs-make         |
| [tools/](./tools)                                 | tool configuration for a nice developer experience |
| [run/](./run)                                     | scripts to illustrate this project                 |
| [pkgs-make/](./pkgs-make)                         | Pkgs-make, the Nix library driving this project    |

The links are presented in loose recommended reading order, starting with tutorials on usage, and ending with the implementation of Pkgs-make. If you don't know what Nix is or the motivation to use it over the alternatives, the first tutorial is a good place to start.

If you don't have a preferred Markdown or Org-mode viewer, we recommend just using Github's Markdown rendering at <https://github.com/shajra/nix-package>.

# Prerequisites<a id="sec-3"></a>

## Knowledge<a id="sec-3-1"></a>

We've tried to make this project explanatory enough that you don't need to read external references first. But covering Nix in its entirety is beyond the scope of this project, so it's really good to have the official documentation available for reference:

-   [Nix Manual](https://nixos.org/nix/manual)
-   [Nixpkgs Manual](https://nixos.org/nixpkgs/manual)

There are also [many good tutorials](https://www.google.com/search?q=nix+tutorial). Here are few that some of us have found useful personally:

-   [Gabriel Gonzalez's haskell-nix](https://github.com/Gabriel439/haskell-nix)
-   [Luca Bruno's "pill" series](http://lethalman.blogspot.com/2014/07/nix-pill-1-why-you-should-give-it-try.html)
-   [John Wiegley's Haskell/Nix talk](https://www.youtube.com/watch?v=G9yiJ7d5LeI)

## Technology<a id="sec-3-2"></a>

### Nix<a id="sec-3-2-1"></a>

Because this project is really about Nix, you should have [the Nix package manager installed](https://nixos.org/nix/manual/#chap-installation).

Unless you're running NixOS as your operating system (which provides Nix intrinsically) the primary way to install Nix on another operating system is by running Nix's install script:

```shell
curl https://nixos.org/nix/install | sh
```

Also, be aware that this project is actively tested on a GNU/Linux OS, and only loosely tested on Macs. Nix support for Macs is always improving, but is known to be idiosyncratic.

If for some reason you can't install Nix, we can still use [Docker](https://www.docker.com) to build some of the projects in the tutorials. We just won't be able to showcase development tools without Nix installed on the host system.

### Docker<a id="sec-3-2-2"></a>

This project illustrates how to build and package up applications in Docker.

See the [Docker installation guide](https://docs.docker.com/install) for instructions on how to install Docker for your system.

### Additional developers tools<a id="sec-3-2-3"></a>

This project also provides some [configuration for some developers tools](./tools) for a nice Nix-based developer experience with this project. Some of us have used these tools on real projects, and we've been pleased with the results.

### Things you don't need to install<a id="sec-3-2-4"></a>

Beyond Docker and these developer tools, you don't need to install language-specific tooling like interpreters/compilers, build systems, or static analysis tools.

Once you have Nix, we can use it as a bootstrap to get all the other tools you might need that are more likely to vary from project to project. Pkgs-make can help with this.

# Feedback Desired<a id="sec-4"></a>

If you have an idea to improve this project's documentation or features, please submit an issue or pull request. Otherwise, feel free to fork/modify it.

This repository has been around for a few years, but we've not yet officially released it to allow it some time to get some heavier refactoring. If you're please interested, please leave an issue suggesting so, and we can take the necessary steps.
