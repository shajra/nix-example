- [About Recommended Developer Tools for Nix](#org42639ba)
- [Quick Start with Spacemacs+Direnv](#org1956b66)



<a id="org42639ba"></a>

# About Recommended Developer Tools for Nix

We want all of Nix's uses and benefits, but without sacrificing the smoothness of our developer experiences.

It really helps to have a few tools configured well for Nix integration. There's many tools, and even more ways to configure them. Here we narrowly only cover one way to configure a small set of tools (for example, Emacs, but not yet Vim). But these are configurations a few people have used actively. We're interested in your feedback if you try them out.

Subdirectories here have Nix-sensitive configuration as follows:

-   **[`direnv`](./direnv/README.md):** to pick up environment settings from nix-shell
-   **[`spacemacs`](./spacemacs/README.md):** a recommended Emacs configuration
-   **[`oh-my-zsh`](./oh-my-zsh/README.md):** Direnv integration with Zsh


<a id="org1956b66"></a>

# Quick Start with Spacemacs+Direnv

The tutorials of this project are set up to work well with the following configuration for a rich Emacs experience.

It's an impractical amount of work to make a script robust enough to install and configure all these tools. There are too many possible preexisting configurations an installation might conflict with.

So here we'll just cover the steps to install and configuration everything except for Oh-My-ZSH and Direnv integration with it.

We'll assume you don't have any conficting configuration. Please be careful.

1.  We'll assumed you've [installed Git](https://git-scm.com/downloads), cloned the repository, and changed to this directory:

    ```shell
    git clone https://github.com/shajra/nix-package
    cd nix-package/tools
    ```

2.  [Install Emacs.](https://www.gnu.org/software/emacs)

3.  [Install Direnv.](https://github.com/direnv/direnv#install)

4.  Copy or link provided Direnv configuration:

    ```shell
    cp direnv/direnvrc ~/.config/direnv/direnvrc
    ```

5.  [Install Spacemacs.](https://github.com/syl20bnr/spacemacs/blob/master/doc/BEGINNERS_TUTORIAL.org#install)

    ```shell
    git clone --branch develop https://github.com/syl20bnr/spacemacs ~/.emacs.d~
    ```

6.  Copy or link all layers into Spacemacs:

    ```shell
    cp -r spacemacs/private/* ~/.emacs.d/private
    ```

7.  Copy or link provided Spacemacs configuration:

    ```shell
    cp spacemacs/dotfile.spacemacs.el ~/.spacemacs
    cp spacemacs/dotfile.spacemacs.static.el ~/.spacemacs.static.el
    ```

8.  Go into two tutorials and enable the Direnv `.envrc` files there:

    ```shell
    ( cd ../tutorials/2-haskell; direnv allow )
    ( cd ../tutorials/2-python; direnv allow )
    ```

With this configuration, you can now run Emacs. It will take a moment to download and load everything the first time you invoke it.

After intialization, you can do things like open [`tutorials/2-haskell/library/src/Lib.hs`](../tutorials/2-haskell/library/src/Lib.hs) and have a Dante-managed GHCi REPL incrementally compile the file using all the tools provided by `nix-shell` via Direnv.

Or you can open [`tutorials/2-python/application/example_app/__main__.py`](../tutorials/2-python/application/example_app/__main__.py) and have Emac's `anaconda-mode` provide features like code navigation, again using and Python environment provided by `nix-shell` via Direnv.
