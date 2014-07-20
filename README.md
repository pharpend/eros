# Eros

A Haskell library for text censorship, using
[DansGuardian Phraselists](http://contentfilter.futuragts.com/phraselists/).

I converted those Phraselists to JSON. You can see the converted Phraselists
[here](https://github.com/pharpend/eros/tree/master/res/phraselists-pretty). There
are
[compressed versions](https://github.com/pharpend/eros/tree/master/res/phraselists-ugly)
for use in your code.

# Contributing

I would love if people would contribute. QuickCheck tests are desperately
needed.

As far as functionality goes, this library is pretty cut & dry. I already added
all of the features I envisioned.

# Versions

Eros is pretty heavy development, so the versions change quickly. I follow the
Hackage standard of `major.minor.even-more-minor.trivial`, where `major` and
`minor` entail API-breaking changes.

In the interest of not confusing myself, I keep Eros and the Eros Client on the
same `major.minor` version. So, a bump in the `major.minor` number doesn't
*necessarily* mean that there's an API-breaking change.

# Contact

The best way to contact me is via IRC. I hang out on `#archlinux` and `#haskell`
on FreeNode. My handles are `l0cust` and `isomorpheous`.
