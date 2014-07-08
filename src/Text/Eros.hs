{-|
Module       : Text.Eros
Description  : Capstone Module for eros
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux

This module serves as a bit of a capstone to the whole eros
library. The idea being you can just import this module, and get
all of the functions from all the rest of eros.

You will have to look in the documentation for the sub-modules for
the functions. I haven't quite figured out how to get the
documentation to show up here yet.

= How to use this library

The basic idea is you take a 'Message' type, and check it against a
'PhraseMap', using 'messageScore'. 'Message' is actually just a type
alias for 'Tl.Text', so just enable the 'OverloadedStrings' extension,
and pretend you're using normal strings.

In GHCi,

>>> :set -XOverloadedStrings 
>>> import Text.Eros

In a file,

> {\-# LANGUAGE OverloadedStrings #-\}
> import Text.Eros

== Constructing 'PhraseMap's

A 'PhraseMap' is just a 'Phraselist' marshaled into the more
Haskell-friendly 'Ms.Map' type.

Eros provides a large number of 'Phraselist's. 

> data ErosList = Chat
>               | Conspiracy
>               | DrugAdvocacy
>               | Forums
>               | Gambling
>               | Games
>               | Gore
>               | IdTheft
>               | IllegalDrugs
>               | Intolerance
>               | LegalDrugs
>               | Malware
>               | Music
>               | News
>               | Nudism
>               | Peer2Peer
>               | Personals
>               | Pornography
>               | Proxies
>               | SecretSocieties
>               | SelfLabeling
>               | Sport
>               | Translation
>               | UpstreamFilter
>               | Violence
>               | WarezHacking
>               | Weapons
>               | Webmail
>   deriving (Eq)


The easiest way to marshal a 'Phraselist' into a 'PhraseMap' is to use
the 'readPhraseMap' function.

> readPhraseMap :: Phraselist t => t -> IO PhraseMap

Use it like this

>>> pornMap <- readPhraseMap Pornography
30

Internally, 'readPhraseMap' reads JSON data containing the
'Phraselist', marshals it into a list of 'PhraseAlmostTree's, converts
those into a 'PhraseForsest', and then into a 'PhraseMap'.

You can obviously use 'mkMap' and 'readPhraselist' to do it yourself,
but it's a lot easier to just use 'readPhraseMap'.

You can then use 'messageScore' to see the 'Score' (actually an 'Int')
of each message.

>>> messageScore "Go fuck yourself." pornMap

'messageScore' is not case sensitive, so @"go fUck YoUrself"@ returns
the same score as @"go fuck yourself"@, and so on.

If you want to use multiple eros lists, do something like this

>>> let myLists = [Chat, Pornography, Weapons]
>>> myMaps <- mapM readPhraseMap myLists
>>> map (messageScore "Go fuck yourself") myMaps
[0, 30, 0]

= Using your own phraselists

I haven't added /good/ support in for this yet, but there still is
support nonetheless. Your phraselist needs to be in JSON, in
accordance with the Phraselist schema (I'm too lazy to find a link to
it). 

> data MyList = MyList
> instance Phraselist MyList where
>   phraselistPath MyList = "/path/to/phraselist"

You can then do the normal stuff with 'messageScore' and 'readPhraseMap'.

-}
module Text.Eros (module Text.Eros) where

import Text.Eros.Message    as Text.Eros
import Text.Eros.Phrase     as Text.Eros
import Text.Eros.Phraselist as Text.Eros

import qualified Data.Map as Ms
import qualified Data.Text.Lazy as Tl
import Text.Eros.Message
import Text.Eros.Phraselist
import Text.Eros.Phrase
