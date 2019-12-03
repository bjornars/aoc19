{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser
  ( module Text.ParserCombinators.ReadP,
    module Parser,
  )
where

import Data.Char (isDigit)
import Data.Maybe (fromJust)
import RIO
import qualified RIO.Text as T
import Text.ParserCombinators.ReadP

nonNegDigit :: ReadP Int
nonNegDigit = (read <$> many1 (satisfy isDigit))
  where
    read = fromJust . readMaybe

digit :: ReadP Int
digit = nonNegDigit <++ (char '-' *> (negate <$> nonNegDigit))

parse :: ReadP a -> Text -> Maybe a
parse parser xs =
  let text = T.unpack xs
   in readP_to_S parser text & \case
        [(res, "")] -> Just res
        _ -> Nothing
