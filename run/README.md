Three tutorials provide accompanying code that can be run as discussed in their respective README files:

-   [1-pkgs-make](../tutorials/1-pkgs-make) (a shell program)
-   [2-haskell](../tutorials/2-haskell)
-   [2-python](../tutorials/2-python)

These programs are simple and just output a message to the terminal.

There are four “run-\*” scripts in this directory that you can run if you have Nix installed. Each takes as a single argument either “shell”, “haskell”, or “python” to call the respective tutorial.

Here's a description of the four scripts:

-   **`run-native`:** runs the program with Nix alone (no Docker involved)

-   **`run-docker-image`:** builds a Docker image with Nix and runs it with Docker

-   **`run-docker-tarball`:** builds tarball with Nix which is built into a Docker image and run with Docker

-   **`run-licences`:** prints a license report with Nix alone (no Docker involved)

If you don't have Nix installed locally, we can do the Nix work within a Nix-imaged Docker container. Just pass the command you want to run as arguments to the provided `docker-nix` script. For instance:

```shell
./docker-nix run-docker-image haskell
```

We tried to keep the code clean, so you may find that you can pick up a lot from reading these scripts, the code in the tutorials, and the Pkgs-make code.
