{-# LANGUAGE OverloadedStrings #-}
module AtxHeader where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative

import Data.Attoparsec.Text hiding (space, skipSpace)
import Utils

data AtxHeader = AtxHeader Int Text deriving (Eq, Ord, Show)

atxHeader = do
    parseLessThan 3 space

    let header n = string (Text.replicate n "#") >> pure n

    headerLevel <- choice . map header $ [6, 5 .. 1]

    requiredSpace

    
    let anyText                = fmap Text.singleton anyChar
        endHeading             = many1 (string "#") >> optionalSpace >> endOfLine
        noEndHeading           = optionalSpace >> endOfLine

        emptyHeaderEnding      = (endHeading <|> noEndHeading) >> pure ""
        textFilledHeaderEnding = (requiredSpace >> endHeading) <|> noEndHeading

        textFilledHeader       = Text.concat <$> manyTill anyText textFilledHeaderEnding

    text <- emptyHeaderEnding <|> textFilledHeader

    return $ AtxHeader headerLevel text

test2 = parseOnly (atxHeader <* endOfInput) "  ###### \245  \n"