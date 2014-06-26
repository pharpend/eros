-- |Some tests for the Text.Eros.Phrase module
module TextErosPhrase where

import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import           Data.Tree
import           Test.QuickCheck
import           Text.Eros.Phrase

-- |Given Text Int pairs, make some phrases
mkPhrases :: [(T.Text, Int)] -> [Phrase]
mkPhrases []       = []
mkPhrases ((s, i):x) = psi:px
  where psi = Phrase s i
        px  = mkPhrases x

-- |Try to order the phrases
ordPhraseForest :: [(T.Text, Int)] -> Bool
ordPhraseForest ti = phraseOrd == scoreOrd
  where scoreOrd  = sort $ map snd ti
        phraseOrd = map score $ sort $ mkPhrases ti
