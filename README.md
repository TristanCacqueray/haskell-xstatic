# XStatic package for Haskell

[![Hackage](https://img.shields.io/hackage/v/xstatic.svg?logo=haskell&label=xstatic)](https://hackage.haskell.org/package/xstatic)
[![Hackage](https://img.shields.io/hackage/v/xstatic-th.svg?logo=haskell&label=xstatic-th)](https://hackage.haskell.org/package/xstatic-th)
[![Hackage](https://img.shields.io/hackage/v/servant-xstatic.svg?logo=haskell&label=servant-xstatic)](https://hackage.haskell.org/package/servant-xstatic)
[![Hackage](https://img.shields.io/hackage/v/lucid-xstatic.svg?logo=haskell&label=lucid-xstatic)](https://hackage.haskell.org/package/lucid-xstatic)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](./xstatic/LICENSE)

Similar to [xstatic-py](https://github.com/xstatic-py/xstatic),
the goal of XStatic family of packages is to provide static file
as Haskell library to be installed using cabal.

## Usage

- Create a XStaticFile using `xstatic-th` or `file-embed`.
- Serve the files using `xstatic` (for wai) or `servant-xstatic` (for servant).
- Add the files to your html page using `lucid-xstatic`.

Checkout the [demo](./demo-xstatic), [demo-xterm](./demo-xterm) or [websockets-ki-htmx](./demo-websockets-ki-htmx) for examples.

- Use this collection by adding this configuration to your `cabal.project`:

```
source-repository-package
    type: git
    location: https://github.com/TristanCacqueray/haskell-xstatic
    tag: 205506d8b53af9f9e448bfd0e2eadec349058eca
    subdir: xstatic-htmx xstatic-tailwind
```

## Notes

- The files are embedded in the code.
- Compressed files are served as-is by automatically adding the `Content-Encoding: gzip` headers.
- When adding new packages, keep the original files license. XStatic only adds metadata.

## Contribute

Contributions and bug reports are welcome!
To work on this project you need a Haskell toolchain: [get-started](https://www.haskell.org/get-started/).

Validate your change with:

```ShellSession
$ cabal build -O0 all --ghc-option=-Werror && cabal test -O0 all
```
