{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE OverloadedStrings #-} 
 
-- |This is the main module for the eros client. This is not the meat &
-- potatoes of the program, but this is the bread & butter.
-- 
module Main where

import           Control.Applicative
import           Control.Monad (mzero)
import           Data.Aeson
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as LzByte
import qualified Data.Text.Lazy         as LzText
import qualified Data.Text.Lazy.IO      as LazyIO
import qualified Data.Map               as M
import qualified System.IO              as StdIO
import           Text.Eros.Message
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
                Nothing  -> "Well, something got fucked up."

-- |Pretty self-explanatory
instance ToJSON (ErosList, Score) where
  toJSON (el, sc) = object [ "eros-list" .= el
                           , "score"     .= sc
                           ]

-- |Pretty self-explanatory
instance ToJSON EroscOutput where
  toJSON (EroscOutput elm) = toJSON elm

main :: IO ()
main = do
  -- take the json from stdin, try to decode it
  inputBt <- LzByte.hGetContents StdIO.stdin
  let eitherJson = (eitherDecode inputBt) :: Either String EroscInput
  -- if by chance, it isn't decoded, the program shall flip its shit
  case eitherJson of
    Left msg      -> fail msg
    Right ecInput -> putStrLn "Yay, you got here!"
  
