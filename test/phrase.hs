{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import           Text.Eros.Phrase
import           Text.Eros.Phraselist
import           Text.Eros.Message


testMessagePhraseScore = do
  pornForest  <- readPhraselist Pornography
  let pornMap     = mkMap pornForest
      pornSayings = forestPhrases pornForest
      brmap msg   = splitAtBadWords msg pornMap
  return ()
