{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE OverloadedStrings #-} 
 
-- |This is the main module for the eros client. This is not the meat &
-- potatoes of the program, but this is the bread & butter.
-- 
module Main where

import           Control.Applicative
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy   as LzByte
import qualified Data.Text.Lazy         as LzText
import qualified Data.Text.Lazy.IO      as LazyIO
import qualified Data.Map               as M
import qualified System.IO              as StdIO
import           Text.Eros.Message
import           Text.Eros.Phrase (mkMap)
import           Text.Eros.Phraselist

-- |It's convenient to think of Scores as Scores, although, they are truly ints.
type Score = Int

-- |We represent the input data in its own data type. This is needed
-- for JSON parsing, but it will also be useful down the road, when I
-- allow input that isn't JSON.
data EroscInput = EroscInput { text      :: LzText.Text
                             , erosLists :: [ErosList]
                             }

-- |This is pretty self-explanatory
instance FromJSON ErosList where
  parseJSON (String s) = case erosListByName (LzText.fromStrict s) of
                           Just list -> return list
                           Nothing   -> mzero
  parseJSON _          = mzero

-- |This is pretty self-explanatory
instance FromJSON EroscInput where
  parseJSON (Object v) = EroscInput
    <$> v .: "text"
    <*> v .: "eros-lists"
  parseJSON _          = mzero

-- |This is the output data type. I will make an instance of ToJSON
-- for this data type. Again, at the start, only JSON input and output
-- is existent.
data EroscOutput = EroscOutput { elScore :: [(ErosList, Score)] }

-- |Pretty self-explanatory
instance ToJSON ErosList where
  toJSON el = case erosNameByList el of
                Just nom -> toJSON nom
                Nothing  -> "There's probably a bug in the JSON parsing \
                            \library Eros uses. You should file a bug \
                            \report at https://github.com/pharpend/eros/issues."

-- |Pretty self-explanatory
instance ToJSON (ErosList, Score) where
  toJSON (el, sc) = object [ "eros-list" .= el
                           , "score"     .= sc
                           ]

-- |Pretty self-explanatory
instance ToJSON EroscOutput where
  toJSON (EroscOutput elm) = toJSON elm

erosEncode :: ToJSON a => a -> LzByte.ByteString
erosEncode = encodePretty' defConfig { confIndent = 2
                                     }
runBtStr :: LzByte.ByteString -> IO ()
runBtStr inputBt = do
  let eitherJson = (eitherDecode inputBt) :: Either String EroscInput
  case eitherJson of
    Left msg      -> fail msg
    Right ecInput -> runInput ecInput

runInput :: EroscInput -> IO ()
runInput ipt = do
  result <- processInput ipt
  let jsonText = erosEncode result
  LzByte.hPutStr StdIO.stdout jsonText

processInput :: EroscInput -> IO EroscOutput
processInput (EroscInput txt lists) = do
    outLists <- mapM listPair lists
    return $ EroscOutput outLists
  where
    listPair :: ErosList -> IO (ErosList, Score)
    listPair ls = do
      pmap <- loadPhraseMap ls
      let scr = messageScore txt pmap
      return (ls, scr)
  
main :: IO ()
main = runBtStr =<< LzByte.hGetContents StdIO.stdin
