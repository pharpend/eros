-- |
-- Module       : Text.Eros.Phrase
-- Description  : Pure interface for 'Phrase's and 'PhraseTree's.
-- Copyright    : 2014, Peter Harpending.
-- License      : BSD3
-- Maintainer   : Peter Harpending <pharpend2@gmail.com>
-- Stability    : experimental
-- Portability  : archlinux
-- 

module Text.Eros.Phrase where

import Data.Ord (comparing)
import Data.Text.Lazy (Text)
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
  -- |To compare 'Phrases', just compare their 'score's
  compare = comparing score

-- |A 'Tree' of 'Phrase's
type PhraseTree = Tree Phrase
-- |A 'Forest' of 'Phrase's
type PhraseForest = Forest Phrase

-- |The 'score' of the 'PhraseTree' is the sum of the 'score's of its 'Node's.
treeScore :: PhraseTree -> Int
treeScore = sum . map score . flatten

-- |Given a list of 'PhraseTree's, return the top-level 'phrase's.
forestPhrases :: [PhraseTree] -> [Text]
forestPhrases = map forestPhrase

-- |Given a 'PhraseTree', return the top-level 'phrase'.
forestPhrase :: PhraseTree -> Text
forestPhrase (Node phr subf) = phrase phr

-- |Given a list of 'PhraseTree's, return an associative list (alist)
-- of each phrase with the appropriate tree.
phraseTreeAlist :: [PhraseTree] -> [(Text, PhraseTree)]
phraseTreeAlist frs = zip (forestPhrases frs) frs
