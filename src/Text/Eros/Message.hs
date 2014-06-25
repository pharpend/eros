{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : Text.Eros.Message
-- Description  : Module for censoring pieces of text.
-- Copyright    : 2014, Peter Harpending
-- License      : BSD3
-- Maintainer   : Peter Harpending <pharpend2@gmail.com>
-- Stability    : experimental
-- Portability  : archlinux
--

module Text.Eros.Message where

import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as L
import           Data.Tree
import           Text.Eros.Phrase
import           Text.Eros.Phraselist

-- |A type alias for text.
type BadWord     = L.Text
type Message     = L.Text
type MessagePart = L.Text
type Restof      = L.Text
type Word        = L.Text

type Score = Int


-- |Given a phrase, this will look up the phrase's score. If the
-- phrase is not listed, this returns 0.
phraseScore :: Message -> (M.Map Message PhraseTree) -> Int
phraseScore msg forestMap  =
  case (M.lookup msg forestMap) of
    Just (Node phrs _) -> score phrs
    Nothing            -> 0
    
-- |Given a message, and a list of potential phrases, find the phrases
-- within the message.
splitAtBadWords :: Message -> PhraseMap -> M.Map BadWord Restof
splitAtBadWords initialText sayingsMap = M.fromList $ concat
                                                    $ filter (/= [])
                                                    $ nub
                                                    $ map breakSaying potentialSayings
  where potentialSayings   = M.keys sayingsMap
        lowerText          = L.toLower initialText
        trimPair (a, b)    = (L.strip a, L.strip b)
        breakSaying saying = map (\(_, b) -> trimPair $ L.splitAt (L.length saying) b)
                                 $ L.breakOnAll saying lowerText
