{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : Text.Eros.Phraselist
-- Description  : A module for Tropical whatever
-- Copyright    : Copyright © 2014 Valkyrian Industries
-- License      : BSD3
-- Maintainer   : Peter Harpending <peter@valkyrian.com>
-- Stability    : experimental
-- Portability  : archlinux
--
-- If you want to make your own phraselist, you need to write a JSON
-- file, in accordance with the
-- <https://gitlab.com/pharpend/eros/raw/master/res/phraselist-schema.json schema>.
-- Once you do that, make a data type for your phraselist.
-- Make your data type an instance of 'Phraselist', and you're good to
-- go.
-- 
-- For example, let's say your phraselist is @mylist.json@, and it's
-- all in accordance with the schema. Your code would look something
-- like this:
-- 
-- @
-- data MyList = MyList
-- 
-- instance Phraselist MyList where
--   phraselistPath MyList = getDataFileName "mylist.json"
--   phraselistPath _      = undefined
-- @
-- 
-- Don't forget to add @mylist.json@ to @Data-Files@ in your @.cabal@
-- file.
-- 

module Text.Eros.Phraselist where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, sequence)
import Data.Aeson
import Data.Text (Text)
import Data.Tree
import Text.Eros.Phrase
import Paths_eros

class Phraselist t where
  phraselistPath :: t -> IO FilePath

-- |The phraselists in @res/@. Each of these constructors correspond
-- to one of the files
-- <https://gitlab.com/pharpend/eros/tree/master/res/phraselists-pretty here>.
-- 
-- Gitlab has a terrible interface, so I won't provide links to each
-- one of them.
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

-- |A list of phraselists we provide.
erosLists :: [ErosList]
erosLists = [ Chat
              , Conspiracy
              , DrugAdvocacy
              , Forums
              , Gambling
              , Games
              , Gore
              , IdTheft
              , IllegalDrugs
              , Intolerance
              , LegalDrugs
              , Malware
              , Music
              , News
              , Nudism
              , Peer2Peer
              , Personals
              , Pornography
              , Proxies
              , SecretSocieties
              , SelfLabeling
              , Sport
              , Translation
              , UpstreamFilter
              , Violence
              , WarezHacking
              , Weapons
              , Webmail
              ]

-- |A list of the paths to the phraselists we provide.
erosListPaths :: IO [FilePath]
erosListPaths = sequence $ map phraselistPath erosLists

-- These are the data paths for the various PhraseLists
instance Phraselist ErosList where
  phraselistPath Chat            = getDataFileName "res/phraselists-ugly/chat.json"
  phraselistPath Conspiracy      = getDataFileName "res/phraselists-ugly/conspiracy.json"
  phraselistPath DrugAdvocacy    = getDataFileName "res/phraselists-ugly/drug-advocacy.json"
  phraselistPath Forums          = getDataFileName "res/phraselists-ugly/forums.json"
  phraselistPath Gambling        = getDataFileName "res/phraselists-ugly/gambling.json"
  phraselistPath Games           = getDataFileName "res/phraselists-ugly/games.json"
  phraselistPath Gore            = getDataFileName "res/phraselists-ugly/gore.json"
  phraselistPath IdTheft         = getDataFileName "res/phraselists-ugly/id-theft.json"
  phraselistPath IllegalDrugs    = getDataFileName "res/phraselists-ugly/illegal-drugs.json"
  phraselistPath Intolerance     = getDataFileName "res/phraselists-ugly/intolerance.json"
  phraselistPath LegalDrugs      = getDataFileName "res/phraselists-ugly/legal-drugs.json"
  phraselistPath Malware         = getDataFileName "res/phraselists-ugly/malware.json"
  phraselistPath Music           = getDataFileName "res/phraselists-ugly/music.json"
  phraselistPath News            = getDataFileName "res/phraselists-ugly/news.json"
  phraselistPath Nudism          = getDataFileName "res/phraselists-ugly/nudism.json"
  phraselistPath Peer2Peer       = getDataFileName "res/phraselists-ugly/peer2peer.json"
  phraselistPath Personals       = getDataFileName "res/phraselists-ugly/personals.json"
  phraselistPath Pornography     = getDataFileName "res/phraselists-ugly/pornography.json"
  phraselistPath Proxies         = getDataFileName "res/phraselists-ugly/proxies.json"
  phraselistPath SecretSocieties = getDataFileName "res/phraselists-ugly/secret-societies.json"
  phraselistPath SelfLabeling    = getDataFileName "res/phraselists-ugly/self-labeling.json"
  phraselistPath Sport           = getDataFileName "res/phraselists-ugly/sport.json"
  phraselistPath Translation     = getDataFileName "res/phraselists-ugly/translation.json"
  phraselistPath UpstreamFilter  = getDataFileName "res/phraselists-ugly/upstream-filter.json"
  phraselistPath Violence        = getDataFileName "res/phraselists-ugly/violence.json"
  phraselistPath WarezHacking    = getDataFileName "res/phraselists-ugly/warez-hacking.json"
  phraselistPath Weapons         = getDataFileName "res/phraselists-ugly/weapons.json"
  phraselistPath Webmail         = getDataFileName "res/phraselists-ugly/webmail.json"

-- |Placeholder type used to read JSON. The JSON schema (currently, at
-- least) is such that one needs this type to read the JSON. You can
-- use 'fromPAT' to convert this type into a 'PhraseTree'
data PhraseAlmostTree = PhraseAlmostTree { patPhrase :: Text
                                         , patScore  :: Int
                                         , patForest :: [PhraseAlmostTree]
                                         }
  deriving (Show, Read)

-- |Alias for 'PhraseAlmostTree'
type PAT = PhraseAlmostTree

-- |You can read the
-- <https://gitlab.com/pharpend/eros/raw/master/res/phraselist-schema.json JSON schema>
-- to see how this works.
instance FromJSON PAT where
  parseJSON (Object v) = PhraseAlmostTree
    <$> v .: "phrase"
    <*> v .: "score"
    <*> v .: "forest"
  parseJSON _ = fail $ "Must be a PAT"

-- |Convert a 'PAT' into a 'PhraseTree'.
fromPAT :: PAT -> PhraseTree
fromPAT (PhraseAlmostTree p s f) = Node (Phrase p s) $ map fromPAT f

-- |I figure some people like to type a lot.
fromPhraseAlmostTree :: PAT -> PhraseTree
fromPhraseAlmostTree = fromPAT
