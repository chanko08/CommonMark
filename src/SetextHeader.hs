{-# LANGUAGE OverloadedStrings #-}
module SetextHeader where
import Control.Applicative

import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Attoparsec.Text hiding (space, skipSpace)

import Utils

data SetextHeader = SetextHeader Int Text deriving (Eq, Ord, Show)

setextHeader = do
    optionalSpaceIndentation >> notSpace
    
    text <- takeTill isEndOfLine
    endOfLine
    
    optionalSpaceIndentation >> notSpace
    
    header <- choice [ 2 <$ many1 (string "-")
                     , 1 <$ many1 (string "=")
                     ]

    skipSpace
    endOfLine
    return $ SetextHeader header text