{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : Text.Trojan.Phrase
-- Description  : Module to get at the various phrase lists
-- Copyright    : Copyright © 2014 Valkyrian Industries
-- License      : BSD3
-- Maintainer   : Peter Harpending <peter@valkyrian.com>
-- Stability    : experimental
-- Portability  : archlinux
--

module Text.Trojan.Phrase where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Tree

-- |A Phrase is a piece of Text, with an int representing its weight.
data Phrase = Phrase { phrase :: Text
                     , score  :: Int
                     }
  deriving (Read, Show)

instance Eq Phrase where
  a == b = (score a) == (score b)
  a /= b = (score a) /= (score b)

instance Ord Phrase where
  -- |To compare Phrases, just compare their scores
  compare = comparing score

-- |A tree of Phrases
type PhraseTree = Tree Phrase
type PhraseList = [PhraseTree]

data PhraseAlmostTree = PhraseAlmostTree { patPhrase :: Text
                                         , patScore  :: Int
                                         , patForest :: [[PhraseAlmostTree]]
                                         }
  deriving (Show, Read)

type PAT = PhraseAlmostTree

instance FromJSON PAT where
  parseJSON (Object v) = PhraseAlmostTree
    <$> v .: "phrase"
    <*> v .: "score"
    <*> v .: "forest"
  parseJSON a = fail $ show a


-- |The score of the tree is the sum of the scores of its nodes
treeScore :: PhraseTree -> Int
treeScore = sum . map score . flatten

fromPAT :: PAT -> PhraseTree
fromPAT (PhraseAlmostTree p s f) =
  Node (Phrase p s) $ map fromPATs f

fromPATs :: [PAT] -> [PhraseTree]
fromPATs = map fromPAT
