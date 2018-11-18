# Demo

First, make sure the dependencies listed below are installed. Then you can build the demo with:

```sh
stack build --install-ghc --allow-different-user
```

Use either of the following to run the demo:

```sh
stack exec implementation-exe
stack exec implementation-exe <path>
```

## Dependencies

- [Stack](https://docs.haskellstack.org/en/stable/README/): This can be installed via `curl -sSL https://get.haskellstack.org/ | sh` on many platforms.
- [GNU Readline](https://tiswww.case.edu/php/chet/readline/rltop.html): This can be
installed via `brew install readline` (macOS) or `apt-get install libreadline-dev` (Debian).

## Troubleshooting

If you get an error when compiling the `readline` package, set the `CFLAGS` and `LDFLAGS` environment variables to help the compiler locate the GNU Readline installation. For example, if you installed Readline via `brew install readline`, then you probably need this:

```
export CFLAGS="$CFLAGS -I/usr/local/opt/readline/include"
export LDFLAGS="$LDFLAGS -L/usr/local/opt/readline/lib"
```
