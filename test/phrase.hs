{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Eros.Phrase
import Text.Eros.Phraselist
import Text.Eros.Message

testMessagePhraseScore = do
  pornForest  <- readPhraselist Pornography
  let pornMap     = phraseTreeMap pornForest
      pornSayings = forestPhrases pornForest
      msgp m      = print $ messageSayings m pornMap
  msgp "Fuck you, you fucking fuck!"
  msgp "Fuck!"
  msgp "Clitty clitty bang bang."
