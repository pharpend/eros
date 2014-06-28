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

# Contributing

If you want to contribute, you'll need `ghc` and `cabal-install` 

0.  Clone the git repo
    ```bash
    git clone https://github.com/pharpend/eros.git
    ```
1.  Install in a sandbox
    ```
    cd eros
    cabal sandbox init
    cabal install --enable-tests
    ```
