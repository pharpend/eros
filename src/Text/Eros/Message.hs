{- |
Module       : Text.Eros.Message
Description  : Module for censoring pieces of text.
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux

This module deals specifically with pieces of Text.
-}

module Text.Eros.Message where

-- Here, we have all the imports.
import           Control.Applicative
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as L
import           Data.Tree
import           Text.Eros.Phrase
import           Text.Eros.Phraselist


-- |I can never remember what I named things, so here are a bunch of
-- type synonyms.
type BadWord      = L.Text
type Message      = L.Text
type MessagePart  = L.Text
type Multiplicity = Int
type Restof       = L.Text
type RestOf       = L.Text
type Word         = L.Text
type Score        = Int
type SubMap       = PhraseMap

-- |Given a message, get all the bad words in the message, along with
-- the rest of the message
badWordsRestof :: Message -> PhraseMap -> [(BadWord, Restof)]
badWordsRestof msg pmap = concat keyLists
  where lowerMsg = L.toLower msg
        mapKeys  = M.keys pmap
        keyLists = [ brokenKeys
                   | key <- mapKeys
                   , let keyTuples = L.breakOnAll key lowerMsg
                         restOfs = map snd keyTuples
                         brokenKeys = map (L.splitAt (L.length key)) restOfs
                   , keyTuples /= []
                   ]

-- |Given a message, get all the bad words in the message, along with
-- their multiplicity.
badWordMults :: Message -> PhraseMap -> M.Map BadWord Multiplicity
badWordMults msg pmap = M.fromList keysInMsg
  where
    keysInMsg = [ brokenKeyPairs
                | key <- mapKeys 
                , let keyTuples      = L.breakOnAll key lowerMsg
                      restOfs        = map snd keyTuples
                      brokenKeys     = map (L.take (L.length key)) restOfs
                      brokenKeyPairs = (head brokenKeys, length brokenKeys)
                , keyTuples /= []
                ]
    lowerMsg  = L.toLower msg
    mapKeys   = M.keys pmap

-- |Message score flat - no-depth score
messageScore_ :: Message -> PhraseMap -> Score
messageScore_ msg pmap = sum [ (getBadWordScore badwd pmap) * mult
                             | (badwd, mult) <- M.toList $ badWordMults msg pmap
                             ]

getBadWordScore :: BadWord -> PhraseMap -> Score
getBadWordScore badwd pmap = case maybeScore of
                               Just sc -> sc
                               Nothing -> 0
  where maybeScore = score <$> rootLabel <$> M.lookup badwd pmap

getBadWordSubMap :: BadWord -> PhraseMap -> SubMap
getBadWordSubMap badwd pmap = case maybeSubMap of
                               Just mp -> mp
                               Nothing -> M.empty
  where maybeSubMap = mkMap <$> subForest <$> M.lookup badwd pmap

badWordsRestofScoreSubm :: Message -> PhraseMap -> [(BadWord, Restof, Score, SubMap)]
badWordsRestofScoreSubm msg pmap = [ (bdwd, rstof, bws, sbm)
                                   | (bdwd, rstof) <- badWordsRestof msg pmap
                                   , let bws = getBadWordScore bdwd pmap
                                         sbm = getBadWordSubMap bdwd pmap
                                   ]

brss :: Message -> PhraseMap -> [(BadWord, Restof, Score, SubMap)]
brss = badWordsRestofScoreSubm

messageScore :: Message -> PhraseMap -> Score
messageScore msg pmap
  | L.empty == msg  = 0
  | M.empty == pmap = 0
  | otherwise       = sum [ scr + lowerScore
                          | (bdw, rof, scr, sbm) <- brss msg pmap
                          , let lowerScore = messageScore rof sbm
                          ]
