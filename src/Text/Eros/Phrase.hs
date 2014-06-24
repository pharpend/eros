-- |
-- Module       : Text.Eros.Phrase
-- Description  : Module to get at the various phrase lists
-- Copyright    : Copyright (c) 2014, Peter Harpending.
-- License      : BSD3
-- Maintainer   : Peter Harpending <pharpend2@gmail.com>
-- Stability    : experimental
-- Portability  : archlinux
-- 
-- Pure interface for phraselists.
-- 

module Text.Eros.Phrase where

import Data.Ord (comparing)
import Data.Text (Text)
import Data.Tree

-- |A Phrase is a piece of Text, with an int representing its
-- weight. These are the used internally within 'eros', in
-- 'Tree's. 
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

-- |The score of the tree is the sum of the scores of its nodes.
treeScore :: PhraseTree -> Int
treeScore = sum . map score . flatten
