
module Main where

import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as LIO
import qualified System.IO         as SIO
import           Text.Eros.Message
import           Text.Eros.Phraselist

pornCheck :: L.Text -> IO L.Text
pornCheck txt = do
  pornMap <- readPhraseMap Pornography
  let scr = messageScore txt pornMap
  return $ L.pack $ show scr
  

main :: IO ()
main = LIO.hGetContents SIO.stdin >>= pornCheck >>= LIO.putStr
