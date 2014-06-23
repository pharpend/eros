module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import System.Exit
import Text.Trojan.Phrase

dcd :: L.ByteString -> Either String [PAT]
dcd = eitherDecode

testPhraseFile :: FilePath -> IO ()
testPhraseFile f = do
  txt <- L.readFile f
  let ejs = dcd txt
  case ejs of
    Left error -> print error >> exitFailure
    Right json -> print $ map fromPAT json
