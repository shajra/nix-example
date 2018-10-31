- [About Nix-configuration for Spacemacs](#orgc816008)
  - [A Small Pitch](#org8b62355)
- [Installation](#org2383a77)
- [Configuration](#org5c64b7b)



<a id="orgc816008"></a>

# About Nix-configuration for Spacemacs

This is an editor configuration some of us have used professionally for a while now, so we feel it's vetted enough to share. There may be other ways to configure other editors. This is just the best way we've found.


<a id="org8b62355"></a>

## A Small Pitch

The [Spacemacs](http://spacemacs.org) project is just an opinionated configuration for [GNU Emacs](https://www.gnu.org/software/emacs). We like the Spacemacs project because it has a very good user experience that provides easier discovery of features for beginners, but doesn't hamper the full expressiveness of Emacs. Also, though Spacemacs does have a lot of code beyond what's provided by plain Emacs, it's not all turned on at once. Even if you enable a lot of Spacemacs layers, they are often lazily loaded upon first use.

Most importantly, Spacemacs code is well-factored. When first picking up Emacs, a lot of people don't know good packages to try out. And as they find them, it's not at all clear how to best organize the Emacs Lisp code that configures Emacs. A lot of people end up with a very long and twisted `~/.emacs.d/init.el` file.

Because Spacemacs code is modular, it makes it easier to share our Emacs configuration in a pluggable way that's more declarative and plays into the design decisions of the Spacemacs project that we like.


<a id="org2383a77"></a>

# Installation

To start with, you need to install Emacs and then Spacemacs. To install Spacemacs, Git is recommended.

Install Emacs with a package manager (Brew, Apt, Nix, etc.) apropos for your operating system.

Then, assuming you don't yet have an Emacs configuration under `~/.emacs.d`, clone the Spacemacs repository with Git to be your Emacs configuration:

```shell
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

Note that the "master" branch is not updated nearly as frequently as the "develop" branch. However, the configuration described here only works with "master" for now.

If you do have an Emacs configuration already, save/move it off to a temporary location first.

You can now invoke `emacs` and get the default Spacemacs experience.


<a id="org5c64b7b"></a>

# Configuration

Spacemacs is extensible with what they call *layers*. This repository distributes five layers. Each is documented individually:

-   [Extn-Spacemacs](./private/extn-spacemacs/README.md)
-   [Extn-Haskell](./private/extn-haskell/README.md)
-   [Extn-Python](./private/extn-python/README.md)
-   [Attrap](./private/attrap/README.md)
-   [Direnv](./private/direnv/README.md)

If you use all of these layers, you can use the provided example Spacemacs configuration files `dotfile.spacemacs.el` and `dotfile.spacemacs.static.el`, which you can copy or link to the following destinations:

```shell
cp dotfile.spacemacs.el ~/.spacemacs
cp dotfile.spacemacs.static.el ~/.spacemacs.static.el
```

If you are already using Spacemacs and have your `.spacemacs` file configured, you can look at these files to see how to best integrate these configurations into what you have. Or move what you have to a temporary location and try what's provided unaltered.
