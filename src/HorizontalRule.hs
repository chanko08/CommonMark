{-# LANGUAGE OverloadedStrings #-}
module HorizontalRule where

import Control.Applicative (many)
import Data.Text (Text)

import Data.Attoparsec.Text hiding (space, skipSpace)

import Utils

data HorizontalRule = HorizontalRule deriving (Show, Eq)

horizontalRule :: Parser HorizontalRule
horizontalRule = parseLessThan 3 space >> ruleChoices >> endOfLine >> return HorizontalRule
    where
        rule s = parseAtLeast 3 $ string s >> skipWhile (== ' ')

        ruleChoices = choice . map rule $ ["-", "_", "*"]