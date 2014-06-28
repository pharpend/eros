-- |
-- Module       : Text.Eros.Message
-- Description  : Module for censoring pieces of text.
-- Copyright    : 2014, Peter Harpending
-- License      : BSD3
-- Maintainer   : Peter Harpending <pharpend2@gmail.com>
-- Stability    : experimental
-- Portability  : archlinux

-- This module deals specifically with pieces of Text.

module Text.Eros.Message where

-- Here, we have all the imports.
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as L
import           Data.Tree
import           Text.Eros.Phrase
import           Text.Eros.Phraselist


-- I can never remember what I named things, so here are a bunch of
-- type synonyms.
type BadWord     = L.Text
type Message     = L.Text
type MessagePart = L.Text
type Restof      = L.Text
type RestOf      = L.Text
type Word        = L.Text
type Score       = Int

messageSplit :: Message -> PhraseMap -> [(Score, RestOf)]
messageSplit initialText sayingsMap = concat $ filter (/= [])
                                                  $ nub
                                                  $ map breakSaying potentialSayings
  where
    potentialSayings      = M.keys sayingsMap
    breakSaying saying    = map (trimSaying saying . snd) $ broked saying
    trimSaying saying txt = (sayScore saying, L.strip $ L.drop (L.length saying) txt)
    sayScore saying       = case maybeScore saying of
                              Just score -> score
                              Nothing    -> 0
    maybeScore saying     = fmap score $ fmap rootLabel $ M.lookup saying sayingsMap
    broked saying         = L.breakOnAll saying lowerText -- looks like [("go ", "fuck yourself ")]
    lowerText             = L.toLower initialText         -- looks like "go fuck yourself" (compared to "gO fUcK yOURSelf")
    trimPair (a, b)       = (L.strip a, L.strip b)        -- looks like ("go", "fuck yourself")

messageScore :: Message -> PhraseMap -> Score
messageScore msg pmap
  | L.empty == msg = 0
  | otherwise      = (sum topScores) + (sum lowerScores)
      where
        msgSplit    = messageSplit msg pmap
        topScores   = map fst msgSplit
        lowerScores = map (\m -> messageScore m pmap) $ map snd msgSplit
