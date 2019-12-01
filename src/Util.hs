{-# LANGUAGE NoImplicitPrelude #-}
module Util where

import RIO
import qualified RIO.Text as T

asFloat :: Integer -> Double
asFloat = fromIntegral

readInt :: Text -> Maybe Integer
readInt = T.unpack >>> readMaybe
