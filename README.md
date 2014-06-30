# Eros

A Haskell library for text censorship, using
[DansGuardian Phraselists](http://contentfilter.futuragts.com/phraselists/).

I converted those Phraselists to JSON. You can see the converted Phraselists
[here](https://github.com/pharpend/eros/tree/master/res/phraselists-pretty). There
are
[compressed versions](https://github.com/pharpend/eros/tree/master/res/phraselists-ugly)
for use in your code.

Eros is still in development, and is not ready to be actually used. If you would
like to contribute, please do.

You can try the
[API documentation on Hackage](http://hackage.haskell.org/package/eros) if you
want to learn how to use the library. Hackage isn't terribly reliable at
successfully building the documentation, so I also publish the documentation on
[GitHub pages](https://pharpend.github.io/eros-haddock)

# Usage - v.0.4.0.1

This is a usage guide for version 0.4.0.1. There will be more up-to-date usage
guides as more versions come, hopefully.

To install, add `eros >=0.4 && <0.5` to the `build-depends` field in your
library's `.cabal` file

You can get all the functions, simply by `import`ing `Text.Eros`.

Hackage seems to be unable to build the API documentation for Eros, but it won't
hurt to check [eros on Hackage](http://hackage.haskell.org/package/eros).  If
that doesn't work, I publish the documentation
[here](https://pharpend.github.io/eros-haddock).

# Contributing

I would love if people would contribute. QuickCheck tests are desperately
needed.

As far as functionality goes, this library is pretty cut & dry. I already added
all of the features I envisioned.

# Contact

The best way to contact me is via IRC. I hang out on `#archlinux` and `#haskell`
on FreeNode. My handles are `l0cust` and `isomorpheous`.
