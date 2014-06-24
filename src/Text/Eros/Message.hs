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

import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import           Text.Eros.Phrase
import           Text.Eros.Phraselist

-- |A type alias for text.
type Message = L.Text

-- Check each word against a 'PhraseForest'.
checkWords :: Message -> PhraseForest -> [Maybe PhraseTree]
checkWords text forest = map (\word -> M.lookup word forestMap) (L.words text)
  where forestMap = phraseTreeMap forest
