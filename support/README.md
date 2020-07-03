The files in this directory support the maintenance of the project. You can safely ignore this directory if you only want to use the project.

There are three scripts in this directory.

-   `dependencies-upgrade`
-   `docs-generate`
-   `test-run`

They are designed to be called with no arguments, and can be called from any working directory, though `dependencies-upgrade` and `docs-generate` both modify the source code in place.

`dependencies-upgrade` updates the dependencies in [./nix/sources.json](./nix/sources.json) with a tool called [Niv](https://github.com/nmattia/niv).

`docs-generate` will execute any `SRC` blocks in Org mode files, modifying them in place. And then it generate GitHub Flavored Markdown files from them.

`test-run` builds and runs a few things things provided by this project.

There's also a `setup.org` file with common setup for all the Org mode files in this project.