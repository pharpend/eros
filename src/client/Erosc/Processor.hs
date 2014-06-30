{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |This is the module which actually processes the marshals the
-- data. As far as erosc goes, this is the meat & potatoes.
-- 
-- Basically, Main.hs was getting to be too long, so I created this
-- file.
-- 
-- Copyright (c) 2014, Peter Harpending <pharpend2@gmail.com>. All
-- Rights Reserved.
-- 
-- I'm supposed to say "All Rights Reserved," but this is actuallly
-- BSD-licensed. So, really you can do whatever you want with
-- this. Have fun!

module Erosc.Processor where

-- One of these days, I'm going to figure out how to cut down on these
-- imports. 
import           Control.Applicative
import           Control.Monad                         (mzero)
import           Data.Aeson                     hiding (encode)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LzByte
import qualified Data.Text.Lazy       as LzText
import           Text.Eros

-- |Okay, here's a function that actually does something. It takes an
-- 'Input' type, and process it.
processInput :: Input -> IO Output
processInput (Input txt lists) = do
    outLists <- mapM listPair lists
    return $ Output outLists
  where
    listPair :: ErosList -> IO (ErosList, Score)
    listPair ls = do
      pmap <- loadPhraseMap ls
      let scr = messageScore txt pmap
      return (ls, scr)

-- |This will print JSON in a pretty matter, using 2-space indents. I
-- have no idea why 4-space indents are the default.
encode :: ToJSON a => a -> LzByte.ByteString
encode = encodePretty' defConfig { confIndent = 2
                                 }

-- |We represent the input data in its own data type. This is needed
-- for JSON parsing, but it will also be useful down the road, when I
-- allow input that isn't JSON.
data Input = Input { text      :: LzText.Text
                   , erosLists :: [ErosList]
                   }

-- |This is pretty self-explanatory
instance FromJSON ErosList where
  parseJSON (String s) = case erosListByName (LzText.fromStrict s) of
                           Just list -> return list
                           Nothing   -> mzero
  parseJSON _          = mzero

-- |This is pretty self-explanatory
instance FromJSON Input where
  parseJSON (Object v) = Input
    <$> v .: "text"
    <*> v .: "eros-lists"
  parseJSON _          = mzero

-- |This is the output data type. I will make an instance of ToJSON
-- for this data type. Again, at the start, only JSON input and output
-- is existent.
data Output = Output { elScore :: [(ErosList, Score)] }

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
instance ToJSON Output where
  toJSON (Output elm) = toJSON elm

-- |It's convenient to think of Scores as Scores, although, they are
-- truly ints.
type Score = Int
