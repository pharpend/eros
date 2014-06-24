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
type Message = L.Text
type Word    = L.Text

-- |Given a phrase, this will look up the phrase's score. If the
-- phrase is not listed, this returns 0.
phraseScore :: Message -> (M.Map Message PhraseTree) -> Int
phraseScore msg forestMap  =
  case (M.lookup msg forestMap) of
    Just (Node phrs _) -> score phrs
    Nothing            -> 0
    
-- |Given a message, and a list of potential phrases, find the phrases
-- within the message.
messageSayings :: Message -> M.Map Message PhraseTree -> M.Map (Message, Message) (Maybe PhraseTree)
messageSayings initialText sayingsMap = M.fromList $ concat
                                                   $ filter (/= [])
                                                   $ nub
                                                   $ map (breakSaying) potentialSayings
  where potentialSayings   = M.keys sayingsMap
        lowerText          = L.toLower initialText
        breakSaying saying = map (\(a, b) -> ((L.strip a,
                                               L.strip $ L.take (L.length saying) b),
                                              M.lookup saying sayingsMap))
                                 $ L.breakOnAll saying lowerText
