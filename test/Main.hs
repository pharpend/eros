module Main where

import           Control.Applicative
import qualified Data.Text.Lazy as T
import           Test.QuickCheck
import           TextErosPhrase

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink xs = T.pack <$> shrink (T.unpack xs)

main :: IO ()
main = do
  quickCheck ordPhraseForest
