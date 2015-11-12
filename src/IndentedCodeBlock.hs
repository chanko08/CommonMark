{-# LANGUAGE OverloadedStrings #-}
module IndentedCodeBlock where
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Attoparsec.Text hiding ( space, skipSpace )
import Control.Applicative
import Utils

data IndentedCodeBlock = IndentedCodeBlock Text deriving (Show, Eq)

indentedText n = do
    (count n space *> pure ()) <|> pure ()
    txt <- takeTill isEndOfLine
    endOfLine
    return txt

indentedCodeBlock = do
    txt <- many1 $ choice
        [ indentedText 4
        , skipSpace >> endOfLine >> pure ""
        ]
    return $ IndentedCodeBlock $ Text.unlines txt