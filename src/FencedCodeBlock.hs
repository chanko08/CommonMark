{-# LANGUAGE OverloadedStrings #-}
module FencedCodeBlock where
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Attoparsec.Text hiding ( space, skipSpace )
import Control.Applicative
import Utils
import IndentedCodeBlock (indentedText)

data FencedCodeBlock = FencedCodeBlock Text Text deriving (Show, Eq)


fence = parseAtLeast 3 (string "~") >> skipSpace



fencedCodeBlock = do
    indentationCount <- length <$> parseLessThan 4 space 
    fence
    infoString <- takeTill isEndOfLine
    endOfLine
    text <- Text.concat <$> manyTill (indentedText indentationCount) (count indentationCount space >> fence)
    
    return $ FencedCodeBlock infoString text