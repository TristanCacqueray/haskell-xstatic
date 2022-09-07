# XStatic package for Haskell

[![Hackage](https://img.shields.io/hackage/v/xstatic.svg?logo=haskell)](https://hackage.haskell.org/package/xstatic)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](./xstatic/LICENSE)

Similar to [xstatic-py](https://github.com/xstatic-py/xstatic),
the goal of XStatic family of packages is to provide static file
as Haskell library to be installed using cabal.

## Usage

- Add xstatic and xstatic-$package to your build-depends.
- Create a wai application with the XStatic package:

```haskell
import Network.Wai qualified as Wai
import XStatic qualified as XStatic
import XStatic.Htmx qualified as XStatic

staticApp :: Wai.Application
staticApp = XStatic.xstaticApp [XStatic.htmx]
```

- Serve this application to provide the static files content.

Checkout the [demo](./demo-xstatic), [demo-xterm](./demo-xterm) or [websockets-ki-htmx](./demo-websockets-ki-htmx) for examples.

## Notes

- The files are embedded in the code.
- Compressed files are served as-is by automatically adding the `Content-Encoding: gzip` headers.
- When adding new packages, keep the original files license. XStatic only adds metadata.
