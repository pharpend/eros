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

# Usage - v.0.5.2.0

This is a usage guide for version 0.5.2.0. There will be more up-to-date usage
guides as more versions come, hopefully.

To install, add `eros >=0.5 && <0.6` to the `build-depends` field in your
library's `.cabal` file

You can get all the functions, simply by `import`ing `Text.Eros`.

Hackage seems to be unable to build the API documentation for Eros, but it won't
hurt to check [eros on Hackage](http://hackage.haskell.org/package/eros).  If
that doesn't work, I publish the documentation
[here](https://pharpend.github.io/eros-haddock).

## Using `Text.Eros`

The basic idea is you take a `Message` type, and check it against a `PhraseMap`,
using `messageScore`. `Message` is actually just a type alias for `Tl.Text`, so
just enable the `OverloadedStrings` extension, and pretend you're using normal
strings.

In GHCi,

`:set -XOverloadedStrings`

`import Text.Eros`

In a file,

``` {.haskell}
 {-# LANGUAGE OverloadedStrings #-}
 import Text.Eros
```

### Constructing `PhraseMap`s

A `PhraseMap` is just a `Phraselist` marshaled into the more Haskell-friendly
`Ms.Map` type.

Eros provides a large number of `Phraselist`s.

``` {.haskell}
 data ErosList = Chat
               | Conspiracy
               | DrugAdvocacy
               | Forums
               | Gambling
               | Games
               | Gore
               | IdTheft
               | IllegalDrugs
               | Intolerance
               | LegalDrugs
               | Malware
               | Music
               | News
               | Nudism
               | Peer2Peer
               | Personals
               | Pornography
               | Proxies
               | SecretSocieties
               | SelfLabeling
               | Sport
               | Translation
               | UpstreamFilter
               | Violence
               | WarezHacking
               | Weapons
               | Webmail
   deriving (Eq)
```

The easiest way to marshal a `Phraselist` into a `PhraseMap` is to use the
`readPhraseMap` function.

``` {.haskell}
 readPhraseMap :: Phraselist t => t -> IO PhraseMap
```

Use it like this

`pornMap <- readPhraseMap Pornography`
`30`

Internally, `readPhraseMap` reads JSON data containing the `Phraselist`,
marshals it into a list of `PhraseAlmostTree`s, converts those into a
`PhraseForsest`, and then into a `PhraseMap`.

You can obviously use `mkMap` and `readPhraselist` to do it yourself, but it's a
lot easier to just use `readPhraseMap`.

You can then use `messageScore` to see the `Score` (actually an `Int`) of each
message.

`messageScore "Go fuck yourself." pornMap`

`messageScore` is not case sensitive, so `go fUck YoUrself` returns the same
score as `go fuck yourself`, and so on.

If you want to use multiple eros lists, do something like this

`let myLists = [Chat, Pornography, Weapons]`

`myMaps <- mapM readPhraseMap myLists`

`map (messageScore "Go fuck yourself") myMaps`
`[0, 30, 0]`

### Using your own phraselists

I haven't added *good* support in for this yet, but there still is support
nonetheless. Your phraselist needs to be in JSON, in accordance with the
Phraselist schema (I'm too lazy to find a link to it).

``` {.haskell}
 data MyList = MyList
 instance Phraselist MyList where
   phraselistPath MyList = "/path/to/phraselist"
```

You can then do the normal stuff with `messageScore` and `readPhraseMap`.

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
